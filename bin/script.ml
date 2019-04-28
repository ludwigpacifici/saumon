open Core
open Saumon.Omnipresent

let run args = In_channel.input_all >> Run.start args
