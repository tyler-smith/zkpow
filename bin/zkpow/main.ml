open Core_kernel
open Zkpow




let () = printf "Verified tick? %b\n" Step.Tick.is_valid
let () = printf "Verified tock? %b\n" Wrap.Tock.is_valid
