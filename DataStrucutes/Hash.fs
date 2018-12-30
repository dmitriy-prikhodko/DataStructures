namespace DataStructures

module Hash = 
    let generate source = System.Math.Abs(source.GetHashCode())

