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

  let log n = eprintf "LOG %d\n" n

  let run _network log_engine =
    let open Signature_lib in
    let open Currency in
    let account_infos = Lazy.force Genesis_ledger.Testnet_postake.accounts in
    let accounts = List.map account_infos ~f:(fun (_sk, account) -> account) in
    let accounts_by_balance =
      List.sort accounts ~compare:(fun acct1 acct2 ->
          Balance.compare acct2.balance acct1.balance )
    in
    let largest_account = List.hd_exn accounts_by_balance in
    let sender = largest_account.public_key |> Public_key.decompress_exn in
    let receiver =
      (List.nth_exn accounts_by_balance 3).public_key
      |> Public_key.decompress_exn
    in
    let fee = Fee.of_int 5 in
    let amount =
      Option.value_exn
        (Amount.sub
           (Balance.to_amount largest_account.balance)
           (Amount.of_int 1_000))
    in
    log 9 ;
    let open Deferred.Or_error.Let_syntax in
    let args = ["get"; "pods"; "--namespace"; "regeneration"] in
    let%bind process = Process.create ~prog:"kubectl" ~args () in
    let reader = Process.stdout process in
    let bytes = Bytes.create 1_000_000 in
    let rec go pos =
      match%bind.Deferred.Let_syntax Reader.read ~pos reader bytes with
      | `Ok n ->
          go (pos + n)
      | `Eof ->
          return
            (let s = Bytes.to_string bytes in
             Core_kernel.eprintf "%s" s ; Ok ())
    in
    let%bind _ = go 0 in
    (* send the payment *)
    let%bind () =
      Node.send_payment ~sender ~receiver amount fee ~memo:"send payment test"
        ()
    in
    Log_engine.wait_for_payment log_engine ~sender ~receiver amount
end
