namespace Shride.Giraffe.Extras

open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open FSharp.Control.Tasks.V2.ContextInsensitive


module HttpHandlers =


    type HttpContext with


        member inline this.SetItem (k, v) =
            this.Items.Add(k, box v)
            this


        member inline this.TryGetItem<'T> k : 'T option =
            match this.Items.TryGetValue(k) with
            | true, v -> v |> (unbox >> Some)
            | false, _ -> None


    let handleCmd<'c> 
        (mapEr : HttpFunc -> HttpContext -> 'er -> HttpFuncResult)
        (handler : IConfiguration -> 'c -> Async<Result<unit, 'er>>) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
             task {
                let! cmd = ctx.BindJsonAsync<'c>()
                let cfg = ctx.GetService<IConfiguration>()
                let! r = handler cfg cmd
                let rs =
                    match r with
                    | Ok _ -> Successful.OK cmd next ctx
                    | Error err -> mapEr next ctx err

                return! rs
             }


    let bindUri<'q> (route : string) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let routeHandler model = 
                fun (next : HttpFunc) (ctx : HttpContext) ->
                    ctx.SetItem(typeof<'q>, model) |> next
            routeBind<'q> route routeHandler next ctx


    let handleQry<'q, 'r, 'er> 
        (mapEr : HttpFunc -> HttpContext -> 'er -> HttpFuncResult)
        (handler : IConfiguration -> 'q -> Async<Result<'r, 'er>>) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
             task {
                let cfg = ctx.GetService<IConfiguration>()
                let handle = 
                    handler cfg >> (fun ar -> task {
                        match! ar with
                        | Ok r -> return Successful.OK r next ctx
                        | Error err -> return mapEr next ctx err
                    })

                let! rs = 
                    ctx.TryGetItem(typeof<'q>)
                    |> Option.map handle
                    |> Option.defaultWith (fun _ -> task {
                            let msg = sprintf "HttpContext does not contain query of type <%s>" typeof<'q>.Name
                            return ServerErrors.INTERNAL_ERROR msg next ctx
                        })

                return! rs
             }