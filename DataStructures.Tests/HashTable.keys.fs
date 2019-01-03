namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module Keys = 

        [<Fact>] 
        let ``should not return any keys for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.keys table = Seq.empty @>

        [<Theory>] 
        [<InlineData(1,2)>]
        let ``should return all keys in the HashTable`` (startKey,endKey) =
            let mutable table = HashTable.empty<int, int>
            for key in {startKey .. endKey} do
                table <- HashTable.add key key table

            test <@  table |> HashTable.keys  |> Seq.toArray = [| startKey .. endKey |]  @>
