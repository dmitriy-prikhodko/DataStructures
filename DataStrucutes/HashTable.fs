namespace DataStructures        


type HashTable<'Key, 'Value when 'Key: comparison and 'Value : equality> (initTable: InternalTable<'Key, 'Value>) = 
    let initialLength = 1000
    let fillFactor = 0.75

    let table = initTable

    member this.Add key value = new HashTable<'Key, 'Value>(InternalTable.add key value initialLength fillFactor table)
    member this.Remove key = new HashTable<'Key, 'Value>(InternalTable.remove key table)

    member this.IsEmpty = InternalTable.isEmpty table
    member this.Count = InternalTable.count table

    member this.Keys = InternalTable.keys table
    member this.Values = InternalTable.values table

    member this.TryFind<'Key, 'Value> (key: 'Key) = InternalTable.tryFind key table
    member this.Find<'Key, 'Value> key : 'Value  = InternalTable.find key table

    member this.ContainsKey key = InternalTable.containsKey key table
    member this.ContainsValue value = InternalTable.containsValue value table


module HashTable = 
    let add (key:'Key) (value: 'Value) (table: HashTable<'Key, 'Value>) : HashTable<'Key, 'Value> = table.Add key value 
    let remove (key: 'Key) (table: HashTable<'Key, 'Value>) : HashTable<'Key, 'Value> = table.Remove key

    let find (key: 'Key) (table: HashTable<'Key, 'Value>): 'Value = table.Find key
    let tryFind (key: 'Key) (table: HashTable<'Key, 'Value>): Option<'Value> = table.TryFind key 
    let containsKey (key: 'Key) (table: HashTable<'Key, 'Value>) : bool = table.ContainsKey key
    let containsValue (value: 'Value) (table: HashTable<'Key, 'Value>): bool = table.ContainsValue value

    let isEmpty (table: HashTable<'Key, 'Value>) : bool = table.IsEmpty
    let count (table: HashTable<'Key, 'Value>) : int = table.Count
    let keys (table: HashTable<'Key, 'Value>) : seq<'Key> = table.Keys
    let values (table: HashTable<'Key, 'Value>) : seq<'Value> = table.Values

    let empty<'Key, 'Value when 'Key: comparison and 'Value : equality> : HashTable<'Key, 'Value> = new HashTable<'Key, 'Value>(InternalTable.empty)