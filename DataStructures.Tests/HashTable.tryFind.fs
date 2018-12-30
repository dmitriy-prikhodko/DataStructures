namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module TryFind =

        [<Fact>] 
        let ``should return None for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.tryFind 0 table = None @>
