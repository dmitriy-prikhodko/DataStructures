namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module Empty = 

        [<Fact>] 
        let ``should create empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.isEmpty table @>
