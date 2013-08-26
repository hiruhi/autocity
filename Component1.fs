
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module AutoCity.BuildCity

type city_tile_t =
    | Road
    | Buliding
    | House
    | Shop
    | Empty
    
type city_tiles_t = {Tiles : city_tile_t [,]; Width : int; Height : int}

let genCityTiles w h : city_tiles_t = {Tiles = Array2D.create w h Empty; Width = w; Height = h}

(*  vnum : numbers of virtucal roads
    hnum : numbers of horizontal roads *)
let putRoads vnum hnum city = 
    let vRoadSpace = city.Height / vnum in
    let hRoadSpace = city.Width / hnum in
    let vRoadInds = Array.map (fun x -> (x - 1) * vRoadSpace) [|1..vnum|] in
    let hRoadInds = Array.map (fun x -> (x - 1) * hRoadSpace) [|1..hnum|] in
    let updateFun hi wi v = if Array.exists (fun x -> hi = x) hRoadInds then Road
                            else if Array.exists (fun x -> wi = x) vRoadInds then Road
                            else v in
    {city with Tiles = Array2D.mapi updateFun city.Tiles}

let cityTile2String t = 
    match t with
    | Empty -> "E"
    | Road -> "R"
    | _ -> " "

let printAr2Di a i = Array.iter (fun j -> Array2D.get a i j |> printf "%s") [|0 .. Array2D.length2 a - 1|]; printfn ""

let printAr2D a = Array.iter (printAr2Di a) [|0 .. Array2D.length1 a - 1|]
                                    
let main () = genCityTiles 10 10 |> putRoads 3 2 |> fun city -> Array2D.map cityTile2String city.Tiles |> printAr2D

main()