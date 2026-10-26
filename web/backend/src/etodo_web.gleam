import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http.{Get, Post}
import gleam/http/cookie
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lustre/attribute
import lustre/element
import lustre/element/html
import mist.{type Connection, type ResponseData}

const session_cookie = "etodo_session"

const list_cookie = "etodo_list"

const session_max_age = 28_800

type Todo {
  Todo(
    uid: String,
    description: String,
    status: String,
    priority: String,
    due_time: String,
    has_sub_todos: Bool,
    done: Bool,
    comment: String,
    owner: String,
    shared_with: String,
    created: String,
    done_time: String,
    progress: String,
    lists: String,
    estimate: String,
    remaining: String,
  )
}

type TodoPageData {
  TodoPageData(
    lists: List(String),
    selected_list: String,
    csrf: String,
    can_edit: Bool,
    filtered_out: Int,
    show_all: Bool,
    search: String,
    todos: List(Todo),
  )
}

type SettingField {
  SettingField(value: String, options: List(String))
}

type SettingsData {
  SettingsData(
    csrf: String,
    filter_status: SettingField,
    filter_prio: SettingField,
    sort_order: SettingField,
    sort_order_sec: SettingField,
  )
}

@external(erlang, "eTodoMistAuth", "login")
fn authenticate(username: String, password: String) -> Result(String, Nil)

@external(erlang, "eTodoMistAuth", "session_user")
fn session_user(token: String) -> Result(String, Nil)

@external(erlang, "eTodoMistAuth", "logout")
fn logout(token: String) -> Nil

@external(erlang, "eTodoMistAuth", "todo_data")
fn todo_data(
  token: String,
  list_name: String,
  show_all: Bool,
) -> Result(String, String)

@external(erlang, "eTodoMistAuth", "set_done")
fn set_done(
  token: String,
  csrf: String,
  uid: String,
  done: Bool,
) -> Result(String, Dynamic)

@external(erlang, "eTodoMistAuth", "update_task")
fn update_task(
  token: String,
  csrf: String,
  uid: String,
  status: String,
  priority: String,
  description: String,
  comment: String,
  progress: String,
  due_time: String,
  estimate: String,
  remaining: String,
) -> Result(Nil, Dynamic)

@external(erlang, "eTodoMistAuth", "settings_data")
fn settings_data(token: String) -> Result(String, String)

@external(erlang, "eTodoMistAuth", "save_settings")
fn save_settings(
  token: String,
  csrf: String,
  status: String,
  prio: String,
  sort_order: String,
  sort_order_sec: String,
) -> Result(Nil, Dynamic)

@external(erlang, "eTodoMistAuth", "create_task")
fn create_task(
  token: String,
  csrf: String,
  list: String,
  status: String,
  priority: String,
  description: String,
  comment: String,
  progress: String,
  due_time: String,
  estimate: String,
  remaining: String,
) -> Result(String, Dynamic)

@external(erlang, "eTodoMistAuth", "delete_task")
fn delete_task(token: String, csrf: String, uid: String) -> Result(Nil, Dynamic)

@external(erlang, "eTodoMist", "read_bootstrap_stylesheet")
fn read_bootstrap_stylesheet() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_stylesheet")
fn read_etodo_stylesheet() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_script")
fn read_etodo_script() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_logo")
fn read_etodo_logo() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_icon")
fn read_etodo_icon() -> Result(BitArray, String)

@external(erlang, "eTodoMistAuth", "status_form")
fn status_form(token: String) -> Result(#(String, Bool), String)

@external(erlang, "eTodoMistAuth", "set_status")
fn set_status(
  token: String,
  csrf: String,
  status: String,
  message: String,
) -> Result(Nil, Dynamic)

@external(erlang, "eTodoMist", "status_json")
fn status_json(user: String) -> Result(String, String)

@external(erlang, "eTodoMist", "read_linked_file")
fn read_linked_file(
  reference: String,
  file_name: String,
) -> Result(#(BitArray, String, String), String)

pub fn start(port: Int, host: String, certfile: String, keyfile: String) {
  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      case request.method, request.path_segments(request) {
        Get, ["assets", "bootstrap.min.css"] ->
          stylesheet_response(read_bootstrap_stylesheet())
        Get, ["assets", "etodo.css"] ->
          stylesheet_response(read_etodo_stylesheet())
        Get, ["assets", "etodo.js"] -> script_response(read_etodo_script())
        Get, ["assets", "etodo-logo.png"] -> logo_response(read_etodo_logo())
        Get, ["assets", "icon-192.png"] -> logo_response(read_etodo_icon())
        Get, ["manifest.json"] -> manifest_response()
        Get, ["file"] -> serve_linked_file(request)
        Get, ["status"] -> serve_status(request)
        Get, ["status.json"] -> serve_status_json(request)
        Post, ["status"] -> save_status_request(request)
        Get, [] -> serve_home(request)
        Get, ["settings"] -> serve_settings(request)
        Post, ["settings"] -> save_settings_request(request)
        Post, ["login"] -> login(request)
        Post, ["logout"] -> logout_request(request)
        Post, ["todo", "done"] -> todo_done(request)
        Post, ["todo", "update"] -> todo_update(request)
        Post, ["todo", "delete"] -> todo_delete(request)
        Post, ["todo", "create"] -> todo_create(request)
        _, _ -> response(404, "Not found")
      }
    }
    |> mist.new
    |> mist.bind(host)
    |> mist.port(port)
    |> mist.with_tls(certfile, keyfile)
    |> mist.start

  process.sleep_forever()
}

fn serve_home(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(token, username)) ->
      case requested_list(request) {
        Ok(#(list_name, explicit)) -> {
          let show_all = wants_all(request)
          let search = query_value(request, "q")
          case load_page(token, username, list_name, show_all, search) {
            Error("invalid_list") if !explicit ->
              case load_page(token, username, "All tasks", show_all, search) {
                Ok(page) -> remember_list(page, "All tasks")
                Error(message) -> page_error(message)
              }
            Ok(page) -> remember_list(page, list_name)
            Error(message) -> page_error(message)
          }
        }
        Error(_) -> response(400, "Invalid list selection")
      }
    Error(_) -> login_page("")
  }
}

fn serve_status_json(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(_, username)) ->
      case status_json(username) {
        Ok(body) ->
          response(200, body)
          |> response.set_header("content-type", "application/json")
          |> response.set_header("cache-control", "no-store")
        Error(_) -> response(500, "Could not read status")
      }
    Error(_) -> response(401, "Not signed in")
  }
}

fn serve_status(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(token, _)) ->
      case status_form(token) {
        Ok(#(csrf, can_edit)) -> status_page(csrf, can_edit)
        Error(_) -> login_page("Session expired. Sign in again.")
      }
    Error(_) -> login_page("")
  }
}

fn save_status_request(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      case set_status(token, field("csrf"), field("status"), field("message")) {
        Ok(Nil) ->
          response.new(303)
          |> response.set_header("location", "/status")
          |> response_data
        Error(_) -> response(400, "Could not change the status")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn status_page(csrf: String, can_edit: Bool) -> Response(ResponseData) {
  let cell = fn(id: String, extra: String) {
    html.div(
      [
        attribute.id(id),
        attribute.attribute("class", extra),
      ],
      [],
    )
  }
  let document =
    html.html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.link([attribute.rel("manifest"), attribute.href("/manifest.json")]),
        html.link([
          attribute.rel("icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.link([
          attribute.rel("apple-touch-icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/bootstrap.min.css"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/etodo.css"),
        ]),
        html.title([], "eTodo status"),
        html.script(
          [attribute.src("/assets/etodo.js"), attribute.attribute("defer", "")],
          "",
        ),
      ]),
      html.body([attribute.attribute("class", "etodo-status")], [
        html.main([attribute.id("status-page")], [
          cell("status-user", ""),
          cell("status-state", ""),
          cell("status-msg", ""),
          cell("status-timer", ""),
          cell("status-timer-desc", ""),
        ]),
        html.footer([attribute.id("status-controls")], [
          html.a([attribute.href("/"), attribute.id("status-back")], [
            html.text("Back to todos"),
          ]),
          case can_edit {
            True ->
              html.details([attribute.id("status-edit")], [
                html.summary([], [html.text("Change status")]),
                status_form_element(csrf),
              ])
            False -> element.none()
          },
        ]),
      ]),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(document))
  |> secure_headers
}

fn status_form_element(csrf: String) -> element.Element(a) {
  html.form(
    [
      attribute.id("status-form"),
      attribute.attribute("action", "/status"),
      attribute.attribute("method", "post"),
    ],
    [
      html.input([
        attribute.type_("hidden"),
        attribute.name("csrf"),
        attribute.value(csrf),
      ]),
      html.select(
        [attribute.name("status"), attribute.id("status-select")],
        list.map(["Available", "Busy", "Away"], fn(name) {
          html.option([attribute.attribute("value", name)], name)
        }),
      ),
      html.input([
        attribute.type_("text"),
        attribute.name("message"),
        attribute.id("status-message"),
        attribute.attribute("maxlength", "200"),
        attribute.attribute("placeholder", "Status message"),
        attribute.attribute("autocomplete", "off"),
      ]),
      html.button([attribute.type_("submit")], [html.text("Set status")]),
    ],
  )
}

fn serve_settings(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(token, username)) ->
      case settings_data(token) {
        Ok(encoded) ->
          case json.parse(encoded, settings_decoder()) {
            Ok(data) -> settings_page(username, data)
            Error(_) -> response(500, "Could not decode settings")
          }
        Error(_) -> login_page("Session expired. Sign in again.")
      }
    Error(_) -> login_page("")
  }
}

fn save_settings_request(
  request: Request(Connection),
) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      case
        save_settings(
          token,
          field("csrf"),
          field("filter_status"),
          field("filter_prio"),
          field("sort_order"),
          field("sort_order_sec"),
        )
      {
        Ok(Nil) ->
          response.new(303)
          |> response.set_header("location", "/")
          |> response_data
        Error(_) -> response(400, "Could not save the settings")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn load_page(
  token: String,
  username: String,
  list_name: String,
  show_all: Bool,
  search: String,
) -> Result(Response(ResponseData), String) {
  use encoded_data <- result.try(todo_data(token, list_name, show_all))
  case json.parse(encoded_data, todo_page_data_decoder()) {
    Ok(data) -> Ok(authenticated_page(username, TodoPageData(..data, search:)))
    Error(_) -> Error("decode")
  }
}

fn page_error(message: String) -> Response(ResponseData) {
  case message {
    "invalid_list" -> response(400, "Unknown todo list")
    "invalid_session" -> login_page("Session expired. Sign in again.")
    "decode" -> response(500, "Could not decode todo data")
    _ -> response(500, "Could not load todo data")
  }
}

fn remember_list(
  page: Response(ResponseData),
  list_name: String,
) -> Response(ResponseData) {
  response.prepend_header(
    page,
    "set-cookie",
    cookie.set_header(
      list_cookie,
      uri.percent_encode(list_name),
      cookie.Attributes(
        max_age: Some(31_536_000),
        domain: None,
        path: Some("/"),
        secure: True,
        http_only: True,
        same_site: Some(cookie.Strict),
      ),
    ),
  )
}

fn login(request: Request(Connection)) -> Response(ResponseData) {
  case form_values(request) {
    Ok(#(username, password)) ->
      case authenticate(username, password) {
        Ok(token) ->
          response.new(303)
          |> response.set_header("location", "/")
          |> response.prepend_header(
            "set-cookie",
            cookie.set_header(
              session_cookie,
              token,
              cookie.Attributes(
                max_age: Some(session_max_age),
                domain: None,
                path: Some("/"),
                secure: True,
                http_only: True,
                same_site: Some(cookie.Strict),
              ),
            ),
          )
          |> response_data
        Error(_) -> login_page("Invalid username or password")
      }
    Error(_) -> response(400, "Invalid login request")
  }
}

fn logout_request(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request) {
    Some(token) -> logout(token)
    None -> Nil
  }

  response.new(303)
  |> response.set_header("location", "/")
  |> response.prepend_header(
    "set-cookie",
    cookie.set_header(
      session_cookie,
      "",
      cookie.Attributes(
        max_age: Some(0),
        domain: None,
        path: Some("/"),
        secure: True,
        http_only: True,
        same_site: Some(cookie.Strict),
      ),
    ),
  )
  |> response_data
}

fn form_fields(
  request: Request(Connection),
) -> Result(List(#(String, String)), Nil) {
  use _ <- result.try(case request.get_header(request, "content-length") {
    Ok(content_length) ->
      case int.parse(content_length) {
        Ok(length) if length >= 0 && length <= 4096 -> Ok(Nil)
        _ -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  })
  use content_type <- result.try(request.get_header(request, "content-type"))
  use _ <- result.try(case string.split(content_type, ";") {
    [media_type, ..] ->
      case string.trim(media_type) {
        "application/x-www-form-urlencoded" -> Ok(Nil)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  })
  use request <- result.try(
    request
    |> mist.read_body(max_body_limit: 4096)
    |> result.replace_error(Nil),
  )
  use body <- result.try(bit_array.to_string(request.body))
  uri.parse_query(body)
}

fn form_values(request: Request(Connection)) -> Result(#(String, String), Nil) {
  use fields <- result.try(form_fields(request))
  use username <- result.try(list.key_find(fields, "username"))
  use password <- result.try(list.key_find(fields, "password"))
  case string.trim(username) == "" || password == "" {
    True -> Error(Nil)
    False -> Ok(#(username, password))
  }
}

fn todo_done(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      let list_name = case field("list") {
        "" -> "All tasks"
        name -> name
      }
      let uid = field("uid")
      case set_done(token, field("csrf"), uid, field("done") == "true") {
        Ok(done_time) ->
          case request.get_header(request, "x-etodo-fetch") {
            Ok(_) -> response(200, done_time)
            Error(_) ->
              response.new(303)
              |> response.set_header(
                "location",
                "/?list=" <> uri.percent_encode(list_name) <> "#task-" <> uid,
              )
              |> response_data
          }
        Error(_) -> response(403, "Could not update the task")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn todo_update(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      let list_name = case field("list") {
        "" -> "All tasks"
        name -> name
      }
      let uid = field("uid")
      case
        update_task(
          token,
          field("csrf"),
          uid,
          field("status"),
          field("priority"),
          field("description"),
          field("comment"),
          field("progress"),
          field("due_time"),
          field("estimate"),
          field("remaining"),
        )
      {
        Ok(Nil) -> redirect_to_task(list_name, uid)
        Error(_) -> response(400, "Could not update the task")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn todo_create(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      let list_name = case field("list") {
        "" -> "All tasks"
        name -> name
      }
      case
        create_task(
          token,
          field("csrf"),
          list_name,
          field("status"),
          field("priority"),
          field("description"),
          field("comment"),
          field("progress"),
          field("due_time"),
          field("estimate"),
          field("remaining"),
        )
      {
        Ok(uid) -> redirect_to_task(list_name, uid)
        Error(_) -> response(400, "Could not create the task")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn todo_delete(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      let list_name = case field("list") {
        "" -> "All tasks"
        name -> name
      }
      case delete_task(token, field("csrf"), field("uid")) {
        Ok(Nil) ->
          case request.get_header(request, "x-etodo-fetch") {
            Ok(_) -> response(200, "")
            Error(_) ->
              response.new(303)
              |> response.set_header(
                "location",
                "/?list=" <> uri.percent_encode(list_name),
              )
              |> response_data
          }
        Error(_) -> response(400, "Could not delete the task")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn redirect_to_task(list_name: String, uid: String) -> Response(ResponseData) {
  response.new(303)
  |> response.set_header(
    "location",
    "/?list=" <> uri.percent_encode(list_name) <> "#task-" <> uid,
  )
  |> response_data
}

fn current_session(
  request: Request(Connection),
) -> Result(#(String, String), Nil) {
  use token <- result.try(
    request
    |> session_cookie_value
    |> option.to_result(Nil),
  )
  use username <- result.try(session_user(token))
  Ok(#(token, username))
}

fn query_value(request: Request(Connection), name: String) -> String {
  case request.query {
    Some(query) ->
      case uri.parse_query(query) {
        Ok(fields) -> result.unwrap(list.key_find(fields, name), "")
        Error(_) -> ""
      }
    None -> ""
  }
}

fn wants_all(request: Request(Connection)) -> Bool {
  case request.query {
    Some(query) ->
      case uri.parse_query(query) {
        Ok(fields) -> list.key_find(fields, "all") == Ok("1")
        Error(_) -> False
      }
    None -> False
  }
}

fn requested_list(
  request: Request(Connection),
) -> Result(#(String, Bool), Nil) {
  let remembered = case cookie_value(request, list_cookie) {
    Some(value) ->
      case uri.percent_decode(value) {
        Ok(name) if name != "" -> #(name, False)
        _ -> #("All tasks", False)
      }
    None -> #("All tasks", False)
  }
  case request.query {
    None -> Ok(remembered)
    Some(query) -> {
      use fields <- result.try(uri.parse_query(query))
      case list.key_find(fields, "list") {
        Ok(list_name) -> Ok(#(list_name, True))
        Error(_) -> Ok(remembered)
      }
    }
  }
}

fn cookie_value(request: Request(Connection), name: String) -> Option(String) {
  let cookies =
    request
    |> request.get_header("cookie")
    |> result.unwrap("")
    |> cookie.parse

  case list.key_find(cookies, name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

fn session_cookie_value(request: Request(Connection)) -> Option(String) {
  let cookies =
    request
    |> request.get_header("cookie")
    |> result.unwrap("")
    |> cookie.parse

  case list.key_find(cookies, session_cookie) {
    Ok(token) -> Some(token)
    Error(_) -> None
  }
}

fn login_page(error_message: String) -> Response(ResponseData) {
  let message = case error_message {
    "" -> []
    _ -> [
      html.div(
        [
          attribute.attribute("class", "alert alert-danger"),
          attribute.attribute("role", "alert"),
        ],
        [html.text(error_message)],
      ),
    ]
  }

  let contents =
    [
      html.div([attribute.attribute("class", "mb-4")], [
        html.div([attribute.attribute("class", "etodo-logo-banner mb-3")], [
          html.img([
            attribute.src("/assets/etodo-logo.png"),
            attribute.alt("eTodo"),
            attribute.attribute("class", "etodo-login-logo"),
          ]),
        ]),
        html.p([attribute.attribute("class", "text-body-secondary mb-0")], [
          html.text("Sign in with your eTodo account."),
        ]),
      ]),
    ]
    |> list.append(message)
    |> list.append([
      html.form(
        [
          attribute.attribute("action", "/login"),
          attribute.attribute("method", "post"),
        ],
        [
          html.div([attribute.attribute("class", "mb-3")], [
            html.label(
              [
                attribute.attribute("class", "form-label fw-semibold"),
                attribute.attribute("for", "username"),
              ],
              [html.text("Username")],
            ),
            html.input([
              attribute.attribute("class", "form-control form-control-lg"),
              attribute.attribute("type", "text"),
              attribute.attribute("id", "username"),
              attribute.name("username"),
              attribute.attribute("autocomplete", "username"),
              attribute.attribute("required", "required"),
            ]),
          ]),
          html.div([attribute.attribute("class", "mb-4")], [
            html.label(
              [
                attribute.attribute("class", "form-label fw-semibold"),
                attribute.attribute("for", "password"),
              ],
              [html.text("Password")],
            ),
            html.input([
              attribute.attribute("class", "form-control form-control-lg"),
              attribute.attribute("type", "password"),
              attribute.attribute("id", "password"),
              attribute.name("password"),
              attribute.attribute("autocomplete", "current-password"),
              attribute.attribute("required", "required"),
            ]),
          ]),
          html.button(
            [
              attribute.attribute("class", "btn btn-primary btn-lg w-100"),
              attribute.attribute("type", "submit"),
            ],
            [html.text("Sign in")],
          ),
        ],
      ),
    ])

  let document =
    html.html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.link([attribute.rel("manifest"), attribute.href("/manifest.json")]),
        html.link([
          attribute.rel("icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.link([
          attribute.rel("apple-touch-icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.meta([attribute.name("theme-color"), attribute.content("#f5f7fb")]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/bootstrap.min.css"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/etodo.css"),
        ]),
        html.title([], "eTodo login"),
      ]),
      html.body([attribute.attribute("class", "etodo-login-page")], [
        html.main(
          [
            attribute.attribute(
              "class",
              "container min-vh-100 d-flex align-items-center justify-content-center py-5",
            ),
          ],
          [
            html.div(
              [attribute.attribute("class", "row justify-content-center w-100")],
              [
                html.div(
                  [
                    attribute.attribute(
                      "class",
                      "col-12 col-sm-10 col-md-7 col-lg-5 col-xl-4",
                    ),
                  ],
                  [
                    html.div(
                      [
                        attribute.attribute(
                          "class",
                          "card border-0 shadow-lg etodo-login-card",
                        ),
                      ],
                      [
                        html.div(
                          [attribute.attribute("class", "card-body p-4 p-md-5")],
                          contents,
                        ),
                      ],
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      ]),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  let status = case error_message {
    "" -> 200
    _ -> 401
  }

  response.new(status)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(document))
  |> secure_headers
}

fn script_response(script: Result(BitArray, String)) -> Response(ResponseData) {
  case script {
    Ok(script) ->
      response.new(200)
      |> response.set_header("content-type", "text/javascript; charset=utf-8")
      |> response.set_body(mist.Bytes(bytes_tree.from_bit_array(script)))
      |> secure_headers
    Error(_) -> response(500, "Could not load script")
  }
}

fn stylesheet_response(
  stylesheet: Result(BitArray, String),
) -> Response(ResponseData) {
  case stylesheet {
    Ok(stylesheet) ->
      response.new(200)
      |> response.set_header("content-type", "text/css; charset=utf-8")
      |> response.set_body(mist.Bytes(bytes_tree.from_bit_array(stylesheet)))
      |> secure_headers
    Error(_) -> response(500, "Could not load stylesheet")
  }
}

fn serve_linked_file(request: Request(Connection)) -> Response(ResponseData) {
  let fields = case request.query {
    Some(query) -> result.unwrap(uri.parse_query(query), [])
    None -> []
  }
  case list.key_find(fields, "reference"), list.key_find(fields, "filename") {
    Ok(reference), Ok(file_name) ->
      case read_linked_file(reference, file_name) {
        Ok(#(data, mime_type, disposition)) ->
          response.new(200)
          |> response.set_header("content-type", mime_type)
          |> response.set_header("content-disposition", disposition)
          |> response.set_body(mist.Bytes(bytes_tree.from_bit_array(data)))
          |> secure_headers
          |> response.set_header("content-security-policy", "sandbox")
        Error(_) -> response(404, "Not found")
      }
    _, _ -> response(404, "Not found")
  }
}

fn manifest_response() -> Response(ResponseData) {
  let body =
    "{\"name\":\"eTodo\",\"short_name\":\"eTodo\",\"start_url\":\"/\",\"display\":\"standalone\",\"background_color\":\"#ffffff\",\"theme_color\":\"#ffffff\",\"icons\":[{\"src\":\"/assets/icon-192.png\",\"sizes\":\"192x192\",\"type\":\"image/png\"}]}"
  response.new(200)
  |> response.set_header("content-type", "application/manifest+json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
  |> secure_headers
}

fn logo_response(logo: Result(BitArray, String)) -> Response(ResponseData) {
  case logo {
    Ok(image) ->
      response.new(200)
      |> response.set_header("content-type", "image/png")
      |> response.set_body(mist.Bytes(bytes_tree.from_bit_array(image)))
      |> secure_headers
    Error(_) -> response(500, "Could not load eTodo logo")
  }
}

fn authenticated_page(
  username: String,
  data: TodoPageData,
) -> Response(ResponseData) {
  let list_options =
    list.map(data.lists, fn(list_name) {
      let attributes = case list_name == data.selected_list {
        True -> [
          attribute.attribute("value", list_name),
          attribute.attribute("selected", "selected"),
        ]
        False -> [attribute.attribute("value", list_name)]
      }
      html.option(attributes, list_name)
    })

  let todo_items = case data.todos {
    [] -> [
      html.li(
        [
          attribute.attribute(
            "class",
            "card border-0 shadow-sm text-center etodo-empty-state",
          ),
        ],
        [
          html.div([attribute.attribute("class", "card-body p-4 p-md-5")], [
            html.div([attribute.attribute("class", "etodo-empty-mark mb-3")], [
              html.text("✓"),
            ]),
            html.h3([attribute.attribute("class", "h5 fw-semibold")], [
              html.text("All clear"),
            ]),
            html.p([attribute.attribute("class", "text-body-secondary mb-0")], [
              html.text("There are no todos in this list yet."),
            ]),
          ]),
        ],
      ),
    ]
    todos -> list.map(todos, todo_item(_, data))
  }

  let document =
    html.html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.link([attribute.rel("manifest"), attribute.href("/manifest.json")]),
        html.link([
          attribute.rel("icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.link([
          attribute.rel("apple-touch-icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.meta([attribute.name("theme-color"), attribute.content("#f5f7fb")]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/bootstrap.min.css"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/etodo.css"),
        ]),
        html.title([], "eTodo"),
        html.script(
          [attribute.src("/assets/etodo.js"), attribute.attribute("defer", "")],
          "",
        ),
      ]),
      html.body([attribute.attribute("class", "etodo-page")], [
        html.header([attribute.attribute("class", "bg-white border-bottom")], [
          html.div(
            [
              attribute.attribute(
                "class",
                "container etodo-container py-3 d-flex align-items-center gap-3",
              ),
            ],
            [
              html.a(
                [
                  attribute.href("/"),
                  attribute.attribute(
                    "class",
                    "etodo-logo-banner etodo-logo-small",
                  ),
                ],
                [
                  html.img([
                    attribute.src("/assets/etodo-logo.png"),
                    attribute.alt("eTodo"),
                  ]),
                ],
              ),
              html.div(
                [
                  attribute.attribute(
                    "class",
                    "ms-auto d-flex align-items-center gap-3",
                  ),
                ],
                [
                  html.span(
                    [
                      attribute.attribute(
                        "class",
                        "small text-body-secondary d-none d-sm-inline",
                      ),
                    ],
                    [
                      html.text("Signed in as "),
                      html.strong([], [html.text(username)]),
                    ],
                  ),
                  html.a(
                    [
                      attribute.href("/status"),
                      attribute.attribute(
                        "class",
                        "btn btn-outline-secondary btn-sm",
                      ),
                    ],
                    [html.text("Status")],
                  ),
                  html.a(
                    [
                      attribute.href("/settings"),
                      attribute.attribute(
                        "class",
                        "btn btn-outline-secondary btn-sm",
                      ),
                    ],
                    [html.text("⚙ Settings")],
                  ),
                  html.form(
                    [
                      attribute.attribute("action", "/logout"),
                      attribute.attribute("method", "post"),
                      attribute.attribute("class", "m-0"),
                    ],
                    [
                      html.button(
                        [
                          attribute.attribute(
                            "class",
                            "btn btn-outline-secondary btn-sm",
                          ),
                          attribute.attribute("type", "submit"),
                        ],
                        [html.text("Sign out")],
                      ),
                    ],
                  ),
                ],
              ),
            ],
          ),
        ]),
        html.main(
          [
            attribute.attribute(
              "class",
              "container etodo-container py-4 py-md-5",
            ),
          ],
          [
            html.section(
              [
                attribute.attribute("class", "card border-0 shadow-sm mb-4"),
                attribute.attribute("aria-label", "Choose a todo list"),
              ],
              [
                html.div(
                  [attribute.attribute("class", "card-body p-3 p-md-4")],
                  [
                    html.form(
                      [
                        attribute.attribute("action", "/"),
                        attribute.attribute("method", "get"),
                        attribute.attribute(
                          "class",
                          "row g-3 align-items-end etodo-list-form",
                        ),
                      ],
                      [
                        html.div([attribute.attribute("class", "col")], [
                          html.label(
                            [
                              attribute.attribute(
                                "class",
                                "form-label fw-semibold",
                              ),
                              attribute.attribute("for", "todo-list"),
                            ],
                            [html.text("Todo list")],
                          ),
                          html.select(
                            [
                              attribute.name("list"),
                              attribute.attribute("id", "todo-list"),
                              attribute.attribute(
                                "class",
                                "form-select form-select-lg",
                              ),
                            ],
                            list_options,
                          ),
                        ]),
                        html.div([attribute.attribute("class", "col-auto")], [
                          html.button(
                            [
                              attribute.attribute("type", "submit"),
                              attribute.attribute(
                                "class",
                                "btn btn-primary btn-lg etodo-list-submit",
                              ),
                              attribute.title("Refresh list"),
                              attribute.attribute("aria-label", "Refresh list"),
                            ],
                            [html.text("↻")],
                          ),
                        ]),
                      ],
                    ),
                  ],
                ),
              ],
            ),
            case data.can_edit {
              True -> new_todo_form(data)
              False -> element.none()
            },
            html.div([attribute.attribute("class", "mb-4")], [
              html.input([
                attribute.attribute("type", "search"),
                attribute.attribute("id", "todo-filter"),
                attribute.attribute("class", "form-control form-control-lg"),
                attribute.attribute("placeholder", "Filter todos"),
                attribute.attribute("aria-label", "Filter todos"),
                attribute.attribute("autocomplete", "off"),
                attribute.value(data.search),
              ]),
            ]),
            html.div(
              [
                attribute.attribute(
                  "class",
                  "d-flex align-items-center justify-content-between gap-3 mb-3",
                ),
              ],
              [
                html.h2(
                  [attribute.attribute("class", "h4 fw-bold mb-0 text-break")],
                  [html.text(data.selected_list)],
                ),
                html.span(
                  [
                    attribute.attribute(
                      "class",
                      "badge rounded-pill text-bg-primary",
                    ),
                    attribute.attribute("id", "todo-count"),
                  ],
                  [
                    html.text(
                      int.to_string(list.length(data.todos)) <> " shown",
                    ),
                  ],
                ),
                filtered_badge(data),
              ],
            ),
            html.ul(
              [
                attribute.attribute("id", "todo-items"),
                attribute.attribute("class", "list-unstyled d-grid gap-3 mb-0"),
              ],
              todo_items,
            ),
          ],
        ),
      ]),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(document))
  |> secure_headers
}

fn settings_select(label: String, name: String, field: SettingField) {
  html.div([attribute.attribute("class", "mb-3")], [
    html.label(
      [
        attribute.attribute("class", "form-label fw-semibold"),
        attribute.attribute("for", "setting-" <> name),
      ],
      [html.text(label)],
    ),
    html.select(
      [
        attribute.name(name),
        attribute.attribute("id", "setting-" <> name),
        attribute.attribute("class", "form-select"),
      ],
      list.map(field.options, fn(option) {
        case option == field.value {
          True ->
            html.option(
              [
                attribute.attribute("value", option),
                attribute.attribute("selected", "selected"),
              ],
              option,
            )
          False -> html.option([attribute.attribute("value", option)], option)
        }
      }),
    ),
  ])
}

fn settings_card(title: String, contents: List(element.Element(a))) {
  html.section([attribute.attribute("class", "card border-0 shadow-sm mb-4")], [
    html.div([attribute.attribute("class", "card-body p-3 p-md-4")], [
      html.h2([attribute.attribute("class", "h5 fw-bold mb-3")], [
        html.text(title),
      ]),
      ..contents
    ]),
  ])
}

fn settings_page(
  username: String,
  data: SettingsData,
) -> Response(ResponseData) {
  let document =
    html.html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.link([attribute.rel("manifest"), attribute.href("/manifest.json")]),
        html.link([
          attribute.rel("icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.link([
          attribute.rel("apple-touch-icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/bootstrap.min.css"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/etodo.css"),
        ]),
        html.title([], "eTodo settings"),
      ]),
      html.body([attribute.attribute("class", "etodo-page")], [
        html.header([attribute.attribute("class", "bg-white border-bottom")], [
          html.div(
            [
              attribute.attribute(
                "class",
                "container etodo-container py-3 d-flex align-items-center gap-3",
              ),
            ],
            [
              html.a(
                [
                  attribute.href("/"),
                  attribute.attribute(
                    "class",
                    "etodo-logo-banner etodo-logo-small",
                  ),
                ],
                [
                  html.img([
                    attribute.src("/assets/etodo-logo.png"),
                    attribute.alt("eTodo"),
                  ]),
                ],
              ),
              html.span(
                [
                  attribute.attribute(
                    "class",
                    "ms-auto small text-body-secondary d-none d-sm-inline",
                  ),
                ],
                [
                  html.text("Signed in as "),
                  html.strong([], [html.text(username)]),
                ],
              ),
            ],
          ),
        ]),
        html.main(
          [
            attribute.attribute(
              "class",
              "container etodo-container py-4 py-md-5",
            ),
          ],
          [
            html.form(
              [
                attribute.attribute("action", "/settings"),
                attribute.attribute("method", "post"),
              ],
              [
                html.input([
                  attribute.attribute("type", "hidden"),
                  attribute.name("csrf"),
                  attribute.attribute("value", data.csrf),
                ]),
                settings_card("Filter settings", [
                  settings_select(
                    "Keep tasks with status",
                    "filter_status",
                    data.filter_status,
                  ),
                  settings_select(
                    "Keep tasks with prio",
                    "filter_prio",
                    data.filter_prio,
                  ),
                ]),
                settings_card("Presentation settings", [
                  settings_select(
                    "Sort order (primary)",
                    "sort_order",
                    data.sort_order,
                  ),
                  settings_select(
                    "Sort order (secondary)",
                    "sort_order_sec",
                    data.sort_order_sec,
                  ),
                ]),
                html.div(
                  [
                    attribute.attribute("class", "d-flex justify-content-end"),
                  ],
                  [
                    html.button(
                      [
                        attribute.attribute("class", "btn btn-primary"),
                        attribute.attribute("type", "submit"),
                      ],
                      [html.text("List tasks")],
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      ]),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(document))
  |> secure_headers
}

const max_title_length = 90

// Line breaks count as spaces; the details section shows the full text.
fn truncate_description(description: String) -> String {
  let text =
    description
    |> string.replace("\r", " ")
    |> string.replace("\n", " ")
    |> string.split(" ")
    |> list.filter(fn(word) { word != "" })
    |> string.join(" ")
  case string.length(text) > max_title_length {
    True -> string.trim_end(string.slice(text, 0, max_title_length)) <> "..."
    False -> text
  }
}

fn todo_item(task: Todo, data: TodoPageData) {
  let title_class = case task.done {
    True -> "etodo-title text-break etodo-done"
    False -> "etodo-title text-break"
  }
  let toggle_id = "details-" <> task.uid

  let title =
    html.span([attribute.attribute("class", title_class)], [
      html.text(truncate_description(task.description)),
    ])

  let toggle_label =
    html.label(
      [
        attribute.attribute("for", toggle_id),
        attribute.attribute("class", "etodo-info"),
        attribute.title("Show details"),
      ],
      [html.text("▾")],
    )

  let row = case data.can_edit {
    True -> [done_toggle(task, data), title, toggle_label]
    False -> [title, toggle_label]
  }

  html.li(
    [
      attribute.attribute("id", "task-" <> task.uid),
      attribute.attribute(
        "data-search",
        task.description <> " " <> task.comment,
      ),
      attribute.attribute("data-uid", task.uid),
      attribute.attribute("class", "card border-0 shadow-sm etodo-task-card"),
    ],
    [
      html.input([
        attribute.attribute("type", "checkbox"),
        attribute.attribute("id", toggle_id),
        attribute.attribute("class", "etodo-toggle"),
        attribute.attribute("aria-label", "Show details"),
      ]),
      html.div(
        [
          attribute.attribute(
            "class",
            "etodo-row d-flex align-items-center gap-3",
          ),
        ],
        row,
      ),
      task_details(task, data),
    ],
  )
}

fn done_toggle(task: Todo, data: TodoPageData) {
  let #(label, next, mark) = case task.done {
    True -> #("Mark as not done", "false", "✓")
    False -> #("Mark as done", "true", "")
  }
  let hidden = fn(name, value) {
    html.input([
      attribute.attribute("type", "hidden"),
      attribute.name(name),
      attribute.attribute("value", value),
    ])
  }

  html.form(
    [
      attribute.attribute("action", "/todo/done"),
      attribute.attribute("method", "post"),
      attribute.attribute("class", "m-0 flex-shrink-0 etodo-done-form"),
    ],
    [
      hidden("uid", task.uid),
      hidden("done", next),
      hidden("csrf", data.csrf),
      hidden("list", data.selected_list),
      html.button(
        [
          attribute.attribute("type", "submit"),
          attribute.attribute("class", case task.done {
            True -> "etodo-check etodo-check-on"
            False -> "etodo-check"
          }),
          attribute.attribute("role", "checkbox"),
          attribute.attribute("aria-checked", case task.done {
            True -> "true"
            False -> "false"
          }),
          attribute.attribute("aria-label", label),
          attribute.title(label),
        ],
        [html.text(mark)],
      ),
    ],
  )
}

fn task_details(task: Todo, data: TodoPageData) {
  let rows =
    [
      detail_row_field("Status", task.status, "status"),
      detail_row("Priority", task.priority),
      detail_row("Shared with", task.shared_with),
      detail_row("Owner", task.owner),
      detail_row("Created", task.created),
      detail_row("Due date", task.due_time),
      detail_row_field("Done time", task.done_time, "done-time"),
      detail_row("Progress", progress_text(task.progress)),
      detail_row("Estimate (h)", task.estimate),
      detail_row("Remaining (h)", task.remaining),
      detail_row("Description", task.description),
      detail_row("Comment", task.comment),
      detail_row("Lists", task.lists),
      detail_row("Subtasks", case task.has_sub_todos {
        True -> "Yes"
        False -> ""
      }),
    ]
    |> list.flatten

  let details = html.dl([attribute.attribute("class", "row small mb-0")], rows)

  case data.can_edit {
    True ->
      html.div([attribute.attribute("class", "etodo-details")], [
        details,
        html.div([attribute.attribute("class", "mt-3 pt-3 border-top")], [
          html.details([], [
            html.summary(
              [attribute.attribute("class", "btn btn-outline-primary btn-sm")],
              [html.text("Edit task")],
            ),
            task_editor(task, data),
          ]),
        ]),
      ])
    False ->
      html.div([attribute.attribute("class", "etodo-details")], [details])
  }
}

fn new_todo_form(data: TodoPageData) {
  let scope = "new"
  let hidden = fn(name, value) {
    html.input([
      attribute.attribute("type", "hidden"),
      attribute.name(name),
      attribute.attribute("value", value),
    ])
  }

  html.details([attribute.attribute("class", "card border-0 shadow-sm mb-4")], [
    html.summary(
      [
        attribute.attribute(
          "class",
          "card-body py-2 px-3 fw-semibold etodo-new-summary",
        ),
      ],
      [html.text("+ Add todo")],
    ),
    html.div([attribute.attribute("class", "card-body pt-0 px-3 pb-3")], [
      html.form(
        [
          attribute.attribute("action", "/todo/create"),
          attribute.attribute("method", "post"),
          attribute.attribute("class", "row g-3"),
        ],
        [
          hidden("csrf", data.csrf),
          hidden("list", data.selected_list),
          edit_select(scope, "Status", "status", "Planning", [
            "Planning",
            "In progress",
            "Done",
            "N/A",
          ]),
          edit_select(scope, "Priority", "priority", "N/A", [
            "Low",
            "Medium",
            "High",
            "N/A",
          ]),
          edit_input(scope, "Due date", "due_time", "", "date"),
          edit_input(scope, "Progress (%)", "progress", "0", "number"),
          edit_input(scope, "Estimate (h)", "estimate", "0", "number"),
          edit_input(scope, "Remaining (h)", "remaining", "0", "number"),
          edit_textarea(scope, "Description", "description", ""),
          edit_textarea(scope, "Comment", "comment", ""),
          html.div([attribute.attribute("class", "col-12")], [
            html.button(
              [
                attribute.attribute("class", "btn btn-primary"),
                attribute.attribute("type", "submit"),
              ],
              [html.text("Create todo")],
            ),
          ]),
        ],
      ),
    ]),
  ])
}

fn task_editor(task: Todo, data: TodoPageData) {
  let hidden = fn(name, value) {
    html.input([
      attribute.attribute("type", "hidden"),
      attribute.name(name),
      attribute.attribute("value", value),
    ])
  }

  let scope = "edit-" <> task.uid

  html.div([attribute.attribute("class", "pt-3")], [
    html.form(
      [
        attribute.attribute("action", "/todo/update"),
        attribute.attribute("method", "post"),
        attribute.attribute("class", "row g-3"),
      ],
      [
        hidden("uid", task.uid),
        hidden("csrf", data.csrf),
        hidden("list", data.selected_list),
        edit_select(scope, "Status", "status", task.status, [
          "Planning",
          "In progress",
          "Done",
          "N/A",
        ]),
        edit_select(scope, "Priority", "priority", task.priority, [
          "Low",
          "Medium",
          "High",
          "N/A",
        ]),
        edit_input(scope, "Due date", "due_time", task.due_time, "date"),
        edit_input(scope, "Progress (%)", "progress", task.progress, "number"),
        edit_input(scope, "Estimate (h)", "estimate", task.estimate, "number"),
        edit_input(
          scope,
          "Remaining (h)",
          "remaining",
          task.remaining,
          "number",
        ),
        edit_textarea(scope, "Description", "description", task.description),
        edit_textarea(scope, "Comment", "comment", task.comment),
        html.div(
          [
            attribute.attribute(
              "class",
              "col-12 d-flex justify-content-between gap-2",
            ),
          ],
          [
            html.button(
              [
                attribute.attribute("class", "btn btn-primary"),
                attribute.attribute("type", "submit"),
              ],
              [html.text("Save changes")],
            ),
            html.button(
              [
                attribute.attribute("class", "btn btn-outline-danger"),
                attribute.attribute("type", "submit"),
                attribute.attribute("form", "delete-" <> task.uid),
              ],
              [html.text("Delete task")],
            ),
          ],
        ),
      ],
    ),
    html.form(
      [
        attribute.attribute("action", "/todo/delete"),
        attribute.attribute("method", "post"),
        attribute.attribute("id", "delete-" <> task.uid),
        attribute.attribute("class", "etodo-delete-form"),
      ],
      [
        hidden("uid", task.uid),
        hidden("csrf", data.csrf),
        hidden("list", data.selected_list),
      ],
    ),
  ])
}

fn edit_input(
  scope: String,
  label: String,
  name: String,
  value: String,
  input_type: String,
) {
  html.div([attribute.attribute("class", "col-md-6")], [
    html.label(
      [
        attribute.attribute("class", "form-label"),
        attribute.attribute("for", scope <> "-" <> name),
      ],
      [
        html.text(label),
      ],
    ),
    html.input([
      attribute.attribute("class", "form-control"),
      attribute.attribute("id", scope <> "-" <> name),
      attribute.name(name),
      attribute.attribute("type", input_type),
      attribute.attribute("value", value),
      attribute.attribute("min", case input_type {
        "number" -> "0"
        _ -> ""
      }),
      attribute.attribute("max", case name {
        "progress" -> "100"
        _ -> ""
      }),
    ]),
  ])
}

fn edit_textarea(scope: String, label: String, name: String, value: String) {
  html.div([attribute.attribute("class", "col-12")], [
    html.label(
      [
        attribute.attribute("class", "form-label"),
        attribute.attribute("for", scope <> "-" <> name),
      ],
      [
        html.text(label),
      ],
    ),
    html.textarea(
      [
        attribute.attribute("class", "form-control"),
        attribute.attribute("id", scope <> "-" <> name),
        attribute.name(name),
        attribute.attribute("rows", "3"),
        attribute.attribute("data-required", case scope {
          "new" -> "1"
          _ -> ""
        }),
      ],
      value,
    ),
  ])
}

fn edit_select(
  scope: String,
  label: String,
  name: String,
  value: String,
  options: List(String),
) {
  let choices =
    options
    |> list.map(fn(option) {
      let attributes =
        [attribute.attribute("value", option)]
        |> list.append(case option == value {
          True -> [attribute.attribute("selected", "")]
          False -> []
        })
      html.option(attributes, option)
    })

  html.div([attribute.attribute("class", "col-md-6")], [
    html.label(
      [
        attribute.attribute("class", "form-label"),
        attribute.attribute("for", scope <> "-" <> name),
      ],
      [
        html.text(label),
      ],
    ),
    html.select(
      [
        attribute.attribute("class", "form-select"),
        attribute.attribute("id", scope <> "-" <> name),
        attribute.name(name),
      ],
      choices,
    ),
  ])
}

fn progress_text(progress: String) -> String {
  case progress {
    "" -> ""
    value -> value <> "%"
  }
}

fn detail_row(label: String, value: String) {
  detail_row_field(label, value, "")
}

fn detail_row_field(label: String, value: String, field: String) {
  let shown = case string.trim(value) {
    "" -> "-"
    value -> value
  }
  [
    html.dt(
      [attribute.attribute("class", "col-5 col-sm-3 text-body-secondary")],
      [html.text(label)],
    ),
    html.dd(
      [
        attribute.attribute("class", "col-7 col-sm-9 text-break"),
        attribute.attribute("data-field", field),
      ],
      [html.text(shown)],
    ),
  ]
}

fn setting_field_decoder() -> decode.Decoder(SettingField) {
  use value <- decode.field("value", decode.string)
  use options <- decode.field("options", decode.list(of: decode.string))
  decode.success(SettingField(value:, options:))
}

fn settings_decoder() -> decode.Decoder(SettingsData) {
  use csrf <- decode.field("csrf", decode.string)
  use filter_status <- decode.field("filter_status", setting_field_decoder())
  use filter_prio <- decode.field("filter_prio", setting_field_decoder())
  use sort_order <- decode.field("sort_order", setting_field_decoder())
  use sort_order_sec <- decode.field("sort_order_sec", setting_field_decoder())
  decode.success(SettingsData(
    csrf:,
    filter_status:,
    filter_prio:,
    sort_order:,
    sort_order_sec:,
  ))
}

fn todo_page_data_decoder() -> decode.Decoder(TodoPageData) {
  use lists <- decode.field("lists", decode.list(of: decode.string))
  use selected_list <- decode.field("selected_list", decode.string)
  use csrf <- decode.field("csrf", decode.string)
  use can_edit <- decode.field("can_edit", decode.bool)
  use todos <- decode.field("todos", decode.list(of: todo_decoder()))
  use filtered_out <- decode.field("filtered_out", decode.int)
  use show_all <- decode.field("show_all", decode.bool)
  decode.success(TodoPageData(
    lists:,
    selected_list:,
    csrf:,
    can_edit:,
    filtered_out:,
    show_all:,
    search: "",
    todos:,
  ))
}

fn todo_decoder() -> decode.Decoder(Todo) {
  use uid <- decode.field("uid", decode.string)
  use description <- decode.field("description", decode.string)
  use status <- decode.field("status", decode.string)
  use priority <- decode.field("priority", decode.string)
  use due_time <- decode.field("due_time", decode.string)
  use has_sub_todos <- decode.field("has_sub_todos", decode.bool)
  use done <- decode.field("done", decode.bool)
  use comment <- decode.field("comment", decode.string)
  use owner <- decode.field("owner", decode.string)
  use shared_with <- decode.field("shared_with", decode.string)
  use created <- decode.field("created", decode.string)
  use done_time <- decode.field("done_time", decode.string)
  use progress <- decode.field("progress", decode.string)
  use lists <- decode.field("lists", decode.string)
  use estimate <- decode.field("estimate", decode.string)
  use remaining <- decode.field("remaining", decode.string)
  decode.success(Todo(
    uid:,
    description:,
    status:,
    priority:,
    due_time:,
    has_sub_todos:,
    done:,
    comment:,
    owner:,
    shared_with:,
    created:,
    done_time:,
    progress:,
    lists:,
    estimate:,
    remaining:,
  ))
}

fn response(status: Int, body: String) -> Response(ResponseData) {
  response.new(status)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
  |> secure_headers
}

fn response_data(response: Response(String)) -> Response(ResponseData) {
  response
  |> response.map(fn(body) { mist.Bytes(bytes_tree.from_string(body)) })
  |> secure_headers
}

fn secure_headers(response: Response(ResponseData)) -> Response(ResponseData) {
  response
  |> response.set_header("cache-control", "no-store")
  |> response.set_header("x-content-type-options", "nosniff")
  |> response.set_header("x-frame-options", "DENY")
  |> response.set_header(
    "content-security-policy",
    "default-src 'none'; script-src 'self'; connect-src 'self'; style-src 'self'; img-src 'self' data:; manifest-src 'self'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'",
  )
}

fn filtered_badge(data: TodoPageData) -> element.Element(a) {
  let base = "?list=" <> uri.percent_encode(data.selected_list)
  case data.show_all, data.filtered_out {
    True, _ ->
      html.a(
        [
          attribute.attribute("class", "badge rounded-pill text-bg-secondary"),
          attribute.attribute("id", "todo-filtered"),
          attribute.href("/" <> base),
          attribute.title("Apply the filter again"),
        ],
        [html.text("showing all - apply filter")],
      )
    False, 0 -> element.none()
    False, n ->
      html.a(
        [
          attribute.attribute("class", "badge rounded-pill text-bg-secondary"),
          attribute.attribute("id", "todo-filtered"),
          attribute.href("/" <> base <> "&all=1"),
          attribute.title("Show the tasks hidden by the filter"),
        ],
        [html.text(int.to_string(n) <> " filtered out")],
      )
  }
}
