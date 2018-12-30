namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module Values = 

        [<Fact>] 
        let ``should not return any values for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.values table = Seq.empty @>
