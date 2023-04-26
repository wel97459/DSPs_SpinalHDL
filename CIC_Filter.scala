package DSPs_SpinalHDL
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.control.Breaks
import scala.collection.mutable.ArrayBuffer

class Comb(idw:Int=8, odw:Int=9, g:Int=1) extends Component
{
    val io = new Bundle {
        val i_div = in Bool()
        val i_data = in SInt(idw bits)
        val o_data = out SInt(odw bits)
    }

    val data_reg = Vec(Reg(SInt(idw bits)) init(0), g+1)
    when(io.i_div){
        data_reg(0) := io.i_data
        for(i <- 1 to g){
            data_reg(i) := data_reg(i-1)
        }
    }
    io.o_data := (io.i_data - data_reg(g-1)).resize(odw)
}

class Integrator(idw:Int=8, odw:Int=9) extends Component
{
    val io = new Bundle {
        val i_data = in SInt(idw bits)
        val o_data = out SInt(odw bits)
    }

    val data_out = Reg(SInt(odw bits)) init(0)

    data_out := (data_out + io.i_data)
    io.o_data := data_out.resize(odw)
}

class Downsampler(dw:Int=8, r:Int=4) extends Component
{
    val io = new Bundle {
        val i_data = in SInt(dw bits)
        val o_data = out SInt(dw bits)
        val div = out Bool()
    }

    val counter =  Counter(r-1)
    val data_out = Reg(SInt(dw bits)) init(0)

    counter.increment()
    io.div := counter.willOverflowIfInc
    when(counter.willOverflowIfInc){
        data_out := io.i_data 
    }
    io.o_data := data_out
}

class CIC_Interpolation(DataWidth:Int=8, ChainLength:Int = 4, Ratio:Int=4,  DifferentialDelay:Int = 1) extends Component
{
    var odw = DataWidth+log2Up(Math.pow(Ratio, ChainLength).toInt / Ratio)+1
    var udw = DataWidth + ChainLength
    println("odw:"+ odw + "  udw:"+ udw)
    val io = new Bundle {
        val i_data = in SInt(DataWidth bits)
        val o_data = out SInt(odw bits)
        val i_div = in Bool()
    }

    val Combs = new ArrayBuffer[Comb]()
    var Comb_odw = 0;
    for(i <- 0 to ChainLength-1){
        if(i == ChainLength-1){
            Comb_odw = DataWidth+i;
        }else{
            Comb_odw = DataWidth+i+1;
        }
        Combs += new Comb(DataWidth+i, Comb_odw, DifferentialDelay)
        if(i!=0){
            Combs(i).io.i_data := Combs(i-1).io.o_data.resized
        }else{
            Combs(i).io.i_data := io.i_data
        }
        Combs(i).io.i_div := io.i_div
    }

    val upsample = (io.i_div) ? Combs(ChainLength-1).io.o_data | 0

    val Integrators = new ArrayBuffer[Integrator]()
    
    var Int_odw = 0;
    var Int_idw = 0;
    for(i <- 0 to ChainLength-1){
        if(i == 0){
            Int_idw = DataWidth+ChainLength-1
        }else{
            Int_idw = Int_odw
        }
        Int_odw = DataWidth+log2Up((Math.pow(2, ChainLength - i).toInt * Math.pow(Ratio, i+1).toInt) / Ratio)

        Integrators += new Integrator(Int_idw, Int_odw)
        if(i==0){
            Integrators(i).io.i_data := upsample 
        } else {
            Integrators(i).io.i_data := Integrators(i-1).io.o_data
        }
    }

    io.o_data := Integrators(ChainLength-1).io.o_data
}

class CIC_Interpolation_Test(DataWidthIn:Int=8, DataWidthOut:Int=8) extends Component
{
    val io = new Bundle {
        val i_data = in SInt(DataWidthIn bits)
        val o_data = out SInt(DataWidthOut bits)
        val tone = in SInt(32 bits)
        val sound_dac_p = out Bool()
        val sound_dac_n = out Bool()
    }

    val dac = new Delta_Sigma_DAC()

    val cic = new  CIC_Interpolation(DataWidthIn,4,8,2)
    val downs = new Downsampler(DataWidthIn, 8)

    downs.io.i_data := io.i_data.resize(DataWidthIn);
    cic.io.i_div := downs.io.div
    cic.io.i_data := downs.io.o_data
    dac.io.dac_in := cic.io.o_data.resize(16).asBits

    io.o_data := cic.io.o_data.resize(DataWidthOut)
    io.sound_dac_p := dac.io.dac_out
    io.sound_dac_n := !dac.io.dac_out
}

object CIC_Interpolation_sim {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new CIC_Interpolation_Test(8,16)
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)
            dut.io.i_data #= 0
            var c = 0;
            var cc = 0;
            var t = 0.0
            var s = 0.1
            var ss = 0.1

            var tone = 100000
            dut.io.tone #= tone
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    dut.clockDomain.waitRisingEdge()

                    t = c.toFloat / 25000000.0
                    s = Math.sin((2 * Math.PI * tone.toFloat * t))*127

                    // if(c % tone == 0){
                    //     if(tone>1000){
                    //         tone -= 1000
                    //         dut.io.tone #= tone
                    //     }
                    // }

                    // if(c > cc){
                    //     if(s>0){
                    //         s = -(1<<2)
                    //     }else{
                    //         s = (1<<2)
                    //     }
                    //     cc = c + (25000000/tone)
                    // }

                    dut.io.i_data #= (s).toInt
  
                    c += 1
                    if(c > 99999){
                        loop.break;
                    }
                }
            }
        }
    }
}