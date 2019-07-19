type Lid = string;;
type Flight = string;;
type Airport = string;;

type Route = (Flight * Airport) list;;
type LuggageCatalogue = (Lid * Route) list;;

let rec findRoute = function
    | lid, (clid, route) when lid = clid -> route
    | _, _ -> failwith("You just failed!")
