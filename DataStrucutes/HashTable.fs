namespace DataStructures        


type HashTable<'Key, 'Value when 'Key: comparison and 'Value : equality> (initTable: HashTableArray<'Key, 'Value>) = 
    let initialLength = 1000
    let fillFactor = 0.75

    let table = initTable

    member this.Add key value = new HashTable<'Key, 'Value>(HashTableArray.add key value initialLength fillFactor table)
    member this.Remove key = new HashTable<'Key, 'Value>(HashTableArray.remove key table)


    member this.IsEmpty = HashTableArray.isEmpty table
    member this.Count = HashTableArray.count table

    member this.Keys = HashTableArray.keys table
    member this.Values = HashTableArray.values table

    member this.TryFind<'Key, 'Value> (key: 'Key) = HashTableArray.tryFind key table
    member this.Find<'Key, 'Value> key : 'Value  = HashTableArray.find key table

    member this.ContainsKey key = HashTableArray.containsKey key table
    member this.ContainsValue value = HashTableArray.containsValue value table


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

    let empty<'Key, 'Value when 'Key: comparison and 'Value : equality> : HashTable<'Key, 'Value> = new HashTable<'Key, 'Value>(HashTableArray.empty)