
type species = string * string ;;

type rates = species list;;

type reactants = species list;;

type products = species list ;;

type reaction = reactants * int * products * rates ;;

type file = reaction list;;
