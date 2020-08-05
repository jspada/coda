open Core_kernel
open Async
open Integration_test_lib

module Make (Engine : Engine_intf) = struct
  open Engine

  type network = Network.t

  type log_engine = Log_engine.t

  let config =
    let open Test_config in
    {default with block_producers= [{balance= "1"}]; num_snark_workers= 0}

  let run _network log_engine =
    let open Network in
    (* choose sender, receiver, similar to old batch payments test *)
    let runtime_config = Runtime_config.Test_configs.transactions in
    let%bind precomputed_values, _runtime_config =
      Genesis_ledger_helper.init_from_config_file ~logger:(Logger.create ())
        ~may_generate:false ~proof_level:None
        (Lazy.force runtime_config)
      >>| Or_error.ok_exn
    in
    let (module Genesis_ledger) = precomputed_values.genesis_ledger in
    let accounts = Lazy.force Genesis_ledger.accounts in
    let keypairs =
      List.map accounts ~f:Genesis_ledger.keypair_of_account_record_exn
    in
    let largest_account_keypair =
      Genesis_ledger.largest_account_keypair_exn ()
    in
    let sender = largest_account_keypair.public_key in
    let _, largest_account =
      List.find_exn accounts ~f:(fun (_, account) ->
          let open Signature_lib.Public_key in
          equal sender
            (decompress_exn account.Coda_base.Account.Poly.public_key) )
    in
    (* choose first keypair that's not the sender *)
    let receiver =
      let not_sender =
        List.find_exn keypairs ~f:(fun kp ->
            not (Signature_lib.Keypair.equal kp largest_account_keypair) )
      in
      not_sender.public_key
    in
    let fee = Currency.Fee.of_int 5 in
    let amount =
      Option.value_exn
        Currency.(
          Amount.sub
            (Balance.to_amount largest_account.balance)
            (Amount.of_int 1_000))
    in
    let open Deferred.Or_error.Let_syntax in
    (* send the payment *)
    let%bind () =
      Node.send_payment ~sender ~receiver amount fee ~memo:"send payment test"
        ()
    in
    Log_engine.wait_for ~blocks:1 ~timeout:(`Slots 30) log_engine
end
