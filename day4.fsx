let md5 = System.Security.Cryptography.MD5.Create()

let rec checkHash n =
    let bytes = System.Text.ASCIIEncoding().GetBytes($"iwrupvqb{n}")
    let hash = md5.ComputeHash(bytes)
    let hashString: string = System.Convert.ToHexString(hash)
    if hashString.StartsWith("000000") then n else
        checkHash (n + 1)

checkHash 1