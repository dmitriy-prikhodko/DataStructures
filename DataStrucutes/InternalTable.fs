namespace DataStructures   

type InternalTable<'Key, 'Value> = 
    | TableEmpty
    | TableArray of InternalTableElement<'Key, 'Value>[]*int


module InternalTable = 

    let private generateIntex key arr = (Hash.generate key) % (Array.length arr)

    let private init length =  TableArray (Array.init length (fun _ -> TableElementEmpty), 0)

    let tryFindItemWithKey key = function
        | TableEmpty -> None
        | TableArray (array, _) -> 
            let index = generateIntex key array
            InternalTableElement.tryFind key array.[index]

    let private containsItemWithKey key table = 
        match tryFindItemWithKey key table with
        | Some _ -> true
        | None -> false

    let private validateKeyForAdding key table = 
        if (containsItemWithKey key table) then 
            raise (System.ArgumentException(sprintf "The table is already contains the key %A" key))
        else
            table

    let private validateKeyForRemoving key table = 
        if (containsItemWithKey key table) then 
            table
        else
            raise (System.ArgumentException(sprintf "The table does not contain the key %A" key))

    let private initIfEmpty initialLength table = 
        match table with
        |TableEmpty -> init initialLength
        |TableArray _ -> table

    let private addItem key value table = 
        match table with
        |TableArray (arr, count) -> 
            let index = generateIntex key arr
            arr.[index] <- InternalTableElement.add key value arr.[index]
            TableArray (arr, count + 1)
        | _ -> table

    let private removeItem key table = 
        match table with
        | TableArray (arr, count) -> 
            let index = generateIntex key arr
            arr.[index] <- InternalTableElement.remove key arr.[index]
            TableArray (arr, count - 1)
        | TableEmpty -> table
        
    let private items = function
        | TableEmpty -> Seq.empty
        | TableArray (array, _) -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.items y)) Seq.empty

    let private resizeIfFillFactorExceeds (fillFactor:float) table = 
        match table with
        | TableArray (arr, count) when count >= int (fillFactor * float arr.Length) -> 
            Seq.fold (fun table (key, value) -> addItem key value table) (init (arr.Length * 2)) (items table)
        | _ -> table

    let private markAsEmptyIfCountIsZero table =
        match table with
        | TableArray (_, count) when count = 0 -> TableEmpty
        | _ -> table       

    let empty = TableEmpty

    let isEmpty = function 
        | TableEmpty -> true
        | _ -> false

    let count = function 
        | TableEmpty -> 0
        | TableArray (_, count) -> count

    let keys = function
        | TableEmpty -> Seq.empty
        | TableArray (array, _) -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.keys y)) Seq.empty

    let values = function
        | TableEmpty -> Seq.empty
        | TableArray (array, _) -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.values y)) Seq.empty

    let tryFind = tryFindItemWithKey

    let find key table = 
        match tryFind key table with
        | Some value -> value
        | None -> raise (System.ArgumentException(sprintf "The key %A is not found" key))

    let containsKey = containsItemWithKey 

    let containsValue value table = 
        match Seq.tryFind (fun x-> x=value) (values table) with
        | Some _ -> true
        | None -> false

    let add key value initialLength fillFactor = 
        validateKeyForAdding key 
        >> initIfEmpty initialLength 
        >> resizeIfFillFactorExceeds fillFactor 
        >> addItem key value

    let remove key = 
        validateKeyForRemoving key
        >> removeItem key
        >> markAsEmptyIfCountIsZero
