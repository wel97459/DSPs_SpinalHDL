package DSPs_SpinalHDL
import spinal.core._
import spinal.lib._
import spinal.core.sim._


/**
 * @brief First Order Delta Sigma DAC
 */
class Delta_Sigma_DAC(dw:Int = 16, os:Int = 3) extends Component
{
    val io = new Bundle {
        val dac_in = in Bits(16 bits)
        val dac_out = out Bool()
    }

    val dac_accumulator = Reg(UInt( dw+os bits)) init(0)

    val dac_in_extended = io.dac_in.resize(dw+os)
    when (io.dac_out){
        dac_accumulator := dac_accumulator + dac_in_extended.asUInt - (1<<dw-1)
    }otherwise{
        dac_accumulator := dac_accumulator + dac_in_extended.asUInt + (1<<dw-1)
    }

    io.dac_out := !dac_accumulator(dw+os-1);
}