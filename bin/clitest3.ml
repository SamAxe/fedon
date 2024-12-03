
let () =
  let a = Fedwiki.V2_actions.Edit
    { date  = 0
    ; type_ = "new_type"
    ; id    = "12345678"
    ; item  = { id = "my_id" }
    }
  in
    Printf.printf "%s\n" (a |> Fedwiki.V2_actions.yojson_of_v2_action |> Yojson.Safe.pretty_to_string )
    (* Shows that this enumeration(/sum type) is encoded as [ "Edit", { ... } ] *)




(*
   Array primitives:
     CreateArray
     InsertItem Before or After
     RemoveItem
     ReplaceItem (could be a remove followed by an insert, but probably want to be more explicit about replacement semantic)
     MoveItem change the positions of items
     DeleteArray

     Arrays are most often accessed by an numerical index or memory offset.
     Arrays can be static (with fixed initial length) or dynamic (with initial 0 length that can grow, ala)

     let array = CreateArray

     <array'> = InsertItemBefore <array> <index> <item>
     <array'> = InsertItemAfter  <array> <index> <item>

     <array'> = MoveItemBefore <array> <index> <index>
     <array'> = MoveItemAfter  <array> <index> <index>

     <array'> = ReplaceItem  <array> <index> <item>

     <array'> = Remove <array> <index>

     <index> can be 1 longer than the # of items in list, such that InsertItemBefore <array> 0 <item> will
             add an item to an empty list, in contrast that InsertItemAfter <array> 0 <item> would be an
             error in a strict interpretation, or a less strict version could also insert at end of list if
             the index is not found.

             Remove <array> <index> where index is >= array length would be an error.

      DeleteArray <array>

  But a Federated Wiki story is more like a list of items than an array, so an API might be like

    CreateListOfItems
    InsertItem Before or after another item
    MoveItem Before or after another item
    RemoveItem from a list
    DeleteListOfItems

    let <list_handle> = CreateListOfItems

    <listOfItems'> = InsertItemBefore   <listOfItems> <list_item_id> <item>
    <listOfItems'> = InsertItemAfter    <listOfItems> <list_item_id> <item>

    <listOfItems'> = MoveItemBefore     <listOfItems> <list_item_id> <list_item_id>
    <listOfItems'> = MoveItemAfter      <listOfItems> <list_item_id> <list_item_id>

    <listOfItems'> = ReplaceItem        <listOfItems> <list_item_id> <item>

    <listOfItems'> = RemoveItemFromList <listOfItems> <list_item_id>

    DeleteListOfItems <list_handle>


    :Note: that the <list_item_id> is independent of any references that might happen to be
    in the <item>'s themselves.  The <list_item_id> could even be generated as a result of a
    specific render and only valid for that particular render, e.g. array indexes could be used
    in this way.

    :Note: It might make semantic sense for two <item>'s contents to be combined, as in join
    two sentences, a sentence from two different <item>'s.  In the transaction log, this will
    be self-contained to inside the <item>.

    :Note: Similarly, it might make sense for an there to be a container concept within an <item>
    and again, that will be taken care of within the <item> payload and shouldn't leak into
    the transaction log.

    :Note: that the <list_item_id> should never be referenced inside an <item>, and similarily
    no data inside an <item> should leak to outside the item in the transaction log.

  There might be a desirable property that concatenating a list of rendered items produces the same result
  as rendering a list of concatenated items.  For example, if all the items are markdown.

  So fundamentally, a FedWikier server is a journaled named dynamic array server.

  The journal is a list of appended to journal_actions.  When played back,
  a list of items (with paired id's) is returned.  The <item> is completely opaque and
  could be anything, including an encrypted, by the client, payload.


  To this point, only a single journaled named dynamic array, but what are operations that
  involve two arrays?

  * "Copying" -- might be inserting a list of actions into another list at a specific point.
                 (There could be several optimizations based on what properties the <list_item_id> hold
                 such as being unique and persistent/consistent.  If that property doesn't hold, then
                 a mapping/translating function would be required, possibly for each action.)
  * "Replacing" -- might be replacing the transaction list associated with the page name with
                   the list from the page associated with the other name.  e.g. the Fork behavior.
                   (Somewhat interestingly, there is fork behavior that is in the transaction list and
                   there is meta data that belongs in the item).
  * "merging"   -- Use heuristics to do a combination of insertion and replacing of items, though
                   this likely needs to be semantically aware of what is in the <item>.
                   In concept, every pair of list actions could be a different behavior and form
                   a different variation of merge.






  :Note: In thinking through the abstractness of the journal/transaction lists and the more
  complicated interactions of two pages, the concept of shadow pages is very similar to the
  staging area concept in Git.  In other words, put the two pages next to each other, create
  a blank shadow page, and then drag and drop items from each page and then fork the shadow
  page.  That replaces the algorithmic parts with "have a human do it" part.

*)


(*
let () =
  let usage_msg = "./clitest3 -action" in
  let action = ref "" in

  let anon_fun _filename =
    ()
  in

  let speclist =
    [ ("-action", Arg.Set_string action, "Select action Add, Edit, ...")]
  in

    Arg.parse speclist anon_fun usage_msg
    ; Printf.printf "action is %s\n" !action
*)

