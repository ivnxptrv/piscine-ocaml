type hour = int

let zero = 12
let add h1 h2 = (h1 + h2) mod 12
let sub h1 h2 = (((h1 - h2) mod 12) + 12) mod 12
