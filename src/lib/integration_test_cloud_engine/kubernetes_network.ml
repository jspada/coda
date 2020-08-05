open Core_kernel
open Async

module Node = struct
  type t = string

  module Pod = struct
    type t = {namespace: string; name: string}

    let node_opt = ref None

    let set ~namespace ~name = node_opt := Some {namespace; name}

    let get () = !node_opt

    let run_coda ~cmd ~subcmd flags =
      if Option.is_none !node_opt then
        failwith "run_coda: node has not been set" ;
      let open Deferred.Or_error.Let_syntax in
      let node = Option.value_exn !node_opt in
      let kubectl_args =
        ["exec"; node.name; "--namespace"; node.namespace; "--"]
      in
      let coda_args = ["coda"; cmd; subcmd] @ flags in
      let args = kubectl_args @ coda_args in
      let%bind process = Process.create ~prog:"kubectl" ~args () in
      match%map.Deferred.Let_syntax Process.wait process with
      | Ok () ->
          Ok ()
      | Error err -> (
        match err with
        | `Exit_non_zero n ->
            failwithf "run_coda: exited with error code %d, %s\n" n
              Unix.Exit.(of_code n |> to_string_hum)
              ()
        | `Signal sgnl ->
            failwithf "run_coda: got signal %d, %s\n" (Signal.to_caml_int sgnl)
              (Signal.to_string sgnl) () )
  end

  let start _ = failwith "TODO"

  let stop _ = failwith "TODO"

  let send_payment ~sender ~receiver (amount : Currency.Amount.t) fee ?nonce
      ?memo () =
    let flags0 =
      [ "-amount"
      ; Currency.Amount.to_string amount
      ; "-sender"
      ; Signature_lib.Public_key.(
          compress sender |> Compressed.to_base58_check)
      ; "-receiver"
      ; Signature_lib.Public_key.(
          compress receiver |> Compressed.to_base58_check)
      ; "-fee"
      ; Currency.Fee.to_string fee ]
    in
    let flags1 =
      Option.value_map memo ~default:flags0 ~f:(fun m -> flags0 @ ["memo"; m])
    in
    let flags =
      Option.value_map nonce ~default:flags1 ~f:(fun n ->
          flags1 @ ["nonce"; Coda_base.Account.Nonce.to_string n] )
    in
    Pod.run_coda ~cmd:"client" ~subcmd:"send-payment" flags
end

type t =
  { constraint_constants: Genesis_constants.Constraint_constants.t
  ; genesis_constants: Genesis_constants.t
  ; block_producers: Node.t list
  ; snark_coordinators: Node.t list
  ; archive_nodes: Node.t list
  ; testnet_log_filter: string }

let all_nodes {block_producers; snark_coordinators; archive_nodes; _} =
  block_producers @ snark_coordinators @ archive_nodes
