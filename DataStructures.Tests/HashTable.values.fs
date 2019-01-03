namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module Values = 

        [<Fact>] 
        let ``should not return any values for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.values table = Seq.empty @>

        [<Theory>] 
        [<InlineData(1,2)>]
        let ``should return all values in the HashTable`` (startKey,endKey) =
            let mutable table = HashTable.empty<int, int>
            for key in {startKey .. endKey} do
                table <- HashTable.add (key+1) key table

            test <@  table |> HashTable.values  |> Seq.toArray = [| startKey .. endKey |]  @>
