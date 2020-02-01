open Core_kernel
open Snarky

module Tick = struct
  module Impl = Snark.Run.Make (Backends.Mnt4.Default) (Unit)
  open! Impl

  let check_product (l : Field.t list) (total : Field.t) () =
    let open Field in
    let checked_total = List.fold ~init:one l ~f:( * ) in
    let witnessed_total = exists Field.typ ~compute:(fun () -> Field.Constant.of_int 120) in
    Assert.equal checked_total total;
    Assert.equal witnessed_total total

  let total = Field.Constant.of_int 120
  let my_list = List.map ~f:Field.Constant.of_int [1; 2; 3; 4; 5]


  let public_input () = Data_spec.[Typ.list ~length:(List.length my_list) Field.typ; Field.typ]

  let keypair = generate_keypair ~exposing:(public_input ()) check_product

  let proof = prove (Keypair.pk keypair) (public_input ()) check_product () my_list total

  let is_valid = verify proof (Keypair.vk keypair) (public_input ()) my_list total
end
