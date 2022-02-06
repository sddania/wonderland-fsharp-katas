// See the file alphabet-cipher.md for detailed information.


type Alphabet = List<char>
let alphabet: Alphabet = Seq.toList "abcdefghijklmnopqrstuvwxyz"
let rotateAlphabet (alp: Alphabet) : Alphabet = alp.Tail @ [ alp.Head ]

let genAlphabetDict (alp1: Alphabet) (alp2: Alphabet) = List.zip alp1 alp2 |> Map

type AlphabetMatrix = Map<char, Map<char, char>>

let rec recursiveGetMatrix (origAlp: Alphabet) (matrix: AlphabetMatrix) (newAlp: Alphabet) =
    if (origAlp <> newAlp) then
        let newM =
            matrix
            |> Map.add newAlp.Head (genAlphabetDict origAlp newAlp)

        recursiveGetMatrix origAlp newM (rotateAlphabet newAlp)
    else
        matrix

let getAlphabetIndexOfIndex (alp: Alphabet) =
    let rec origRec = recursiveGetMatrix alp
    let firstElement = genAlphabetDict alp alp
    let matrix = Map([ (alp.Head, firstElement) ])
    rotateAlphabet alp |> (origRec matrix)

let alphabetMatrix = getAlphabetIndexOfIndex alphabet

type Message = string
type Keyword = string

let repeatKeyAsMessageLenght (key: Keyword) (message: Message) : Keyword =
    let numberOfrepetition = (message.Length / key.Length)

    let repeatedKey =
        key |> String.replicate numberOfrepetition

    if repeatedKey.Length <> message.Length then
        let lengthStringToAdd = message.Length - repeatedKey.Length
        System.String.Concat(repeatedKey, key.Substring(0, lengthStringToAdd))
    else
        repeatedKey

let mapTwoWords key message (funct: char * char -> char) =
    let longKeyAsNeeded =
        repeatKeyAsMessageLenght key message |> Seq.toList

    let zippedKeys =
        List.zip (message |> Seq.toList) longKeyAsNeeded

    List.map funct zippedKeys |> System.String.Concat

let getEncodeValue (c1, c2) = alphabetMatrix.[c1].[c2]

let getDecodeValue (decodeChar, keyToFind) =
    Map.findKey
        (fun _ value ->
            value
            |> Map.exists (fun key2 value2 -> value2 = decodeChar && key2 = keyToFind))
        alphabetMatrix

let getKeyFromCipher (cipherChar, decodedChar) =
    alphabetMatrix
    |> Map.find cipherChar
    |> Map.findKey (fun _ value -> value = decodedChar)

let rec getFirstWordIfSpecular (repeatedWord: string) (loop: int) =
    let firstPart = repeatedWord.Substring(0, loop)
    let secondPart = repeatedWord.Substring(loop, loop)

    if (firstPart = secondPart) then
        firstPart
    else
        getFirstWordIfSpecular repeatedWord (loop + 1)

let recursiveFoundKey (word: string) =
    let rec key = getFirstWordIfSpecular word 1
    key

let encode (key: Keyword) (message: Message) : Message = mapTwoWords key message getEncodeValue

let decode (key: Keyword) (message: Message) : Message = mapTwoWords key message getDecodeValue

let decipher (cipher: Message) (message: Message) : Keyword =
    mapTwoWords cipher message getKeyFromCipher
    |> recursiveFoundKey

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
