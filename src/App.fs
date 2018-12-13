module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL

type Item = 
    | Todo of string
    | Done of string

type TodoList = Item list

type DoneList = Item list

type Model =
    { ItemForm : string 
      Todos : TodoList
      Dones : DoneList }

type Msg = 
| UpdateItemForm of string
| CreateTodo
| MoveToDone of Item
| DeleteItem of Item

let init() : Model =
    { ItemForm = ""
      Todos = []
      Dones = [] }

let update (msg : Msg) (model : Model) =
    match msg with
    | UpdateItemForm content -> 
        { model with ItemForm = content }
    | CreateTodo -> failwith "not implemented"
    | MoveToDone item -> failwith "not implemented"
    | DeleteItem item -> failwith "not implemented"

// VIEW (rendered with React)

open Fulma

let toCardRow row =
    Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ] row


let toReactElementList dispatch (items : Item list) =
        [Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str "title" ] ]
              Card.content []
                [ Content.content [] [ str "Unfortunately this draft has been rejected 🙁" ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> ()) ] ]
                    [ str "Delete" ] ] ] ]
                    ;
        Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str "title" ] ]
              Card.content []
                [ Content.content [] [ str "Unfortunately this draft has been rejected 🙁" ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> ()) ] ]
                    [ str "Delete" ] ] ] ]
        ]
let doneContainer dispatch (dones : DoneList) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ 
              Card.header []
                [ Card.Header.title [] [ str "Dones" ] ]
              Card.content []
                [ Content.content [] [ 
                    yield! toReactElementList dispatch dones                   
                 ] ]   
            ] 
        ]



let todoContainer dispatch (todos : TodoList) = 
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ 
              Card.header []
                [ Card.Header.title [] [ str "ToDos" ] ]
              Card.content []
                [ Content.content [] [ 
                    yield! toReactElementList dispatch todos                   
                 ] ]   
            ] 
        ]



let toReactElements dispatch (todos : TodoList) (dones : DoneList)=
    [toCardRow [todoContainer dispatch todos; doneContainer dispatch dones]]


let view (model:Model) dispatch =   
    div []
      [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ str "ToDo List" ] ] ]
        Container.container [ Container.IsFluid ]
          [ h1 [ Class "is-size-1 app-title" ] [ str "Manage your time, you dummy!" ]
            Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                [ yield Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ]
                    [ Tile.tile [ Tile.IsChild ]
                        [ Card.card []
                            [ Card.header []
                                [ Card.Header.title [] [ str "Write a new ToDo-item!" ] ]
                              Card.content []
                                [ Input.text [ Input.Placeholder "Your ToDo-item"
                                               Input.Value model.ItemForm
                                               Input.OnChange (fun ev -> UpdateItemForm ev.Value |> dispatch)
                                               Input.Option.Props
                                                 [] ] ]
                              Card.footer []
                                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> dispatch CreateTodo) ] ]
                                    [ str "Submit" ] ] ] ] ]
                  yield! toReactElements dispatch model.Todos model.Dones] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// App
Program.mkSimple init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
 |> Program.withConsoleTrace
//|> Program.withDebugger
#endif
|> Program.run
