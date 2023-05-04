package DSPs_SpinalHDL

import spinal.core._
import spinal.core.sim._
import scala.util.control.Breaks
import scala.collection.mutable.ArrayBuffer

class Sigma_Delta_Modulator(dw:Int = 16, exa:Int = 4) extends Component
{
    val io = new Bundle
    {
        val i_data = in SInt(dw bits)
        val output = out Bool()
        val tone = in UInt(32 bits)
    }

    //Registers
    val Delay1 = Reg(SInt(dw+exa bits)) init(0)
    val Sum1 = Reg(SInt(dw+exa bits)) init(0)
    val Delay2 = Reg(SInt(dw+exa bits)) init(0)
    val Sum2 = Reg(SInt(dw+exa bits)) init(0)

    //Signals
    val data_in_ext = io.i_data.resize(dw+exa)

    val output = Delay2(dw+exa-1);

    // val fb = SInt(dw+exa bits)
    // when(output){
    //     fb := -((1<<dw+exa-2)-1)
    // } otherwise {
    //     fb := ((1<<dw+exa-2)-1)
    // }

    Sum1 := data_in_ext + output.asSInt.resized
    Sum2 := ((Sum1|>>1) + (Delay2|>>1)) - Delay1

    Delay1 := Sum1
    Delay2 := Sum2
    
    io.output := output
}

object Sigma_Delta_Modulator_sim {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new Sigma_Delta_Modulator()
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
            var I = 0.1
            var Q = 0.1
            var Tm = 0.1
            var Tm1 = 0.1
            var arr = new ArrayBuffer[Int]()
            var f0 = 3000000.0
            var tone = f0 / 4.0
            dut.io.tone #= tone.toInt
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    dut.clockDomain.waitRisingEdge()

                    arr += dut.io.output.toBoolean.toInt

                    t = c.toFloat / 3000000.0
                    s = Math.sin((2 * Math.PI * tone * t))*((1<<14)-1).toFloat
                    ss = Math.cos((2 * Math.PI * tone * t))*((1<<14)-1).toFloat

                    //Tm = Math.sin(2 * Math.PI * 1900.0 * t)
                    //Tm1 = Math.sin(2 * Math.PI * 700.0 * t)
                    I = Math.sin(2 * Math.PI * 100.0 * t) * 1
                    Q = Math.cos(2 * Math.PI * 100.0 * t) * 1

                    dut.io.i_data #= ((s * I) + (ss * Q)).toInt
                    //tone -= 100.0
                    dut.io.tone #= c

                    c += 1
                    if(c > 99999){
                        loop.break;
                    }
                }
            }

            val csvString = arr.mkString("\n")
            import java.nio.file.{Paths, Files}
            import java.nio.charset.StandardCharsets
            Files.write(Paths.get("output.csv"), csvString.getBytes(StandardCharsets.UTF_8))
        }
    }
}