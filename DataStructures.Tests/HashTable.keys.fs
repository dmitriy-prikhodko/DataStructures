namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module Keys = 

        [<Fact>] 
        let ``should not return any keys for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.keys table = Seq.empty @>
