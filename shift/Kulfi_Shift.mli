open Kulfi_Types

module type ShiftModel = sig
    val model : topology -> vertex -> demands -> demands
end

module NS : ShiftModel
