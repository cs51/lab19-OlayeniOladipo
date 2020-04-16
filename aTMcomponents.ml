(* C *)

let id = 0 

(* Possible actions that an ATM customer can perform *)
let action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
let account_spec = {name : string; id : id; balance : int} ;;

let ref database = {name : string list; id : id list; balance : int list} ;; 
let accounts = ref [] ;;
(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)
let initialize2 (lst: account_spec list) : () = 
  let rec aux (l: account_spec list) (n: string list) (i: int list) (b: int list) 
            : string list * int list * int list =
    match l with
    | [] -> n, i, b
    | hd :: tl -> aux tl (n @ [hd.name]) (i @ [hd.id]) (b @ [hd.balance]) in
  let n, i, b = aux lst [] [] [] in
  database := {name = n ; id = i; balance = b} ;;

let initialize (lst: account_spec list) : () = 
  accounts := lst ;;

(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let rec acquire_id () : id = 
  Printf.printf "Enter customer id: ";
  let id = read_int () in 
  if String.length (string_of_int id) <> 6 then Printf.printf "Invalid id"; acquire_id ()
  else Printf.printf "Welcome %s"; 

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let acquire_amount () : int = 
  Printf.printf "Enter amount: ";
  read_int () ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let acquire_act () : action = 
  Printf.printf "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit:";
  let ac = read_st in
  match ac with
  | "B" -> Balance
  | "-" -> Withdraw
  | "+" -> Deposit 
  | "=" -> Done 
  | "X" -> Exit ;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id. 
 *)
  
(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance (id : int) : int = ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (id: int) : string ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance : id -> int -> unit ;;

(*....................................................................
  Presenting information and cash to the customer
 *)
  
(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
let present_message : string -> unit ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
let deliver_cash : int -> unit ;;