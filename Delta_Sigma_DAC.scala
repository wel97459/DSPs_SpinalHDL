package DSPs_SpinalHDL
import spinal.core._
import spinal.lib._
import spinal.core.sim._


/**
 * @brief First Order Delta Sigma DAC
 */
class Delta_Sigma_DAC_FOrder(dw:Int = 16) extends Component
{
    val io = new Bundle {
        val dac_in = in UInt(dw bits)
        val dac_out = out Bool()
    }

    val dac_accumulator = Reg(UInt(dw+1 bits)) init(0)

    val dac_in_extended = (io.dac_in(dw-1) ## io.dac_in).asUInt

    when (!io.dac_out){
        dac_accumulator := dac_accumulator + dac_in_extended - (1<<(dw-1))
    }otherwise{
        dac_accumulator := dac_accumulator + dac_in_extended + (1<<(dw-1))
    }

    io.dac_out := dac_accumulator(dw); 
}

/**
 * @brief Second Order Delta Sigma DAC
 */
class Delta_Sigma_DAC_SOrder(dw:Int = 16) extends Component
{
    val io = new Bundle {
        val dac_in = in UInt(dw bits)
        val dac_out = out Bool()
    }

    val dac_accumulator_f = Reg(UInt(dw+2 bits)) init(0)
    val dac_accumulator_s = Reg(UInt(dw+2 bits)) init(0)

    val dac_in_extended = (io.dac_in(dw-1) ## io.dac_in(dw-1) ## io.dac_in).asUInt
    val sum = UInt(dw+2 bits)

    when (!io.dac_out){
        sum := dac_accumulator_f + dac_in_extended - (1<<(dw-1))
    }otherwise{
        sum := dac_accumulator_f + dac_in_extended + (1<<(dw-1))
    }

    dac_accumulator_f := sum
    dac_accumulator_s := sum


    io.dac_out := dac_accumulator_s(dw); 
}