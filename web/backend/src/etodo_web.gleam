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
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam/string_tree
import gleam/uri
import lustre/attribute
import lustre/element
import lustre/element/html
import mist.{type Connection, type ResponseData}

const session_cookie = "etodo_session"

const oidc_cookie = "etodo_oidc"

const list_cookie = "etodo_list"

const messages_cookie = "etodo_messages"

const session_max_age = 28_800

const desktop_cookie = "etodo_desktop"

// Earlier builds persisted this with Path=/work-log, which shadowed newer cookies.
const legacy_desktop_cookie = "etodo_worklog"

fn instance_cookie(request: Request(Connection), base_name: String) -> String {
  // The TCP relay preserves the public Host port but forwards to the owner's web port.
  let host = request.get_header(request, "host") |> result.unwrap("")
  let port = case uri.parse("https://" <> host) {
    Ok(uri.Uri(host: Some(_), port: Some(port), ..))
      if port > 0 && port <= 65_535
    -> port
    Ok(uri.Uri(host: Some(_), port: None, ..)) -> 443
    _ -> 443
  }
  base_name <> "_" <> int.to_string(port)
}

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
    lists_db: List(String),
    estimate: String,
    remaining: String,
    reminder: Reminder,
  )
}

type Reminder {
  Reminder(
    enabled: Bool,
    date: String,
    time: String,
    recurrence: String,
    end_date: String,
    next: String,
  )
}

const no_reminder =
  Reminder(
    enabled: False,
    date: "",
    time: "",
    recurrence: "none",
    end_date: "",
    next: "",
  )

/// The repeat choices of the reminder dialog in the desktop client.
const recurrences = [
  #("none", "Once"),
  #("time15", "Every 15 minutes"),
  #("time30", "Every 30 minutes"),
  #("time1h", "Every hour"),
  #("time2h", "Every 2 hours"),
  #("time4h", "Every 4 hours"),
  #("timeDay", "Every day"),
  #("timeWeek", "Every week"),
  #("timeMonth", "Every month"),
  #("timeYear", "Every year"),
]

type TodoPageData {
  TodoPageData(
    lists: List(String),
    selected_list: String,
    csrf: String,
    can_edit: Bool,
    can_reorder: Bool,
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

@external(erlang, "eWorkLogView", "report_page")
fn report_page(
  token: String,
  report: String,
  date: String,
  list_name: String,
) -> Result(String, String)

@external(erlang, "eWorkLogView", "listener_started")
fn desktop_listener_started(owner: process.Pid, port: Int) -> Nil

@external(erlang, "eWorkLogView", "valid_host")
fn desktop_host_allowed(host: String) -> Bool

@external(erlang, "eWorkLogView", "bootstrap")
fn desktop_bootstrap_token(ticket: String) -> Result(String, Nil)

@external(erlang, "eWorkLogView", "desktop_report_page")
fn desktop_report_page(
  token: String,
  report: String,
  date: String,
  list_name: String,
  background: String,
  foreground: String,
  accent: String,
) -> Result(String, String)

@external(erlang, "eWorkLogView", "desktop_messages_page")
fn desktop_messages_page(
  token: String,
  kind: String,
  filter: String,
  peers: String,
  background: String,
  foreground: String,
  accent: String,
) -> Result(String, String)

@external(erlang, "eWorkLogView", "markdown_html")
fn markdown_html(text: String) -> String

@external(erlang, "eWorkLogView", "markdown_inline_html")
fn markdown_inline_html(text: String) -> String

@external(erlang, "eWorkLogView", "markdown_token")
fn new_markdown_token() -> String

@external(erlang, "eWorkLogView", "messages_page")
fn messages_page(token: String, kind: String) -> Result(String, String)

@external(erlang, "eWorkLogView", "desktop_messages_fragment")
fn desktop_messages_fragment(
  token: String,
  filter: String,
  peers: String,
) -> Result(String, String)

@external(erlang, "eWorkLogView", "desktop_message_action")
fn desktop_message_action(
  token: String,
  csrf: String,
  action: String,
  value: String,
) -> Result(String, String)

@external(erlang, "eWorkLogView", "desktop_user")
fn desktop_user(token: String) -> Result(String, Nil)

@external(erlang, "eTodoPush", "subscribe")
fn push_subscribe(user: String, subject: process.Subject(String)) -> Nil

@external(erlang, "eWorkLogView", "messages_summary")
fn messages_summary(token: String, since: String) -> Result(String, String)

@external(erlang, "eTodoMistAuth", "send_message")
fn send_message(
  token: String,
  csrf: String,
  to: String,
  message: String,
) -> Result(Nil, Dynamic)

@external(erlang, "eWorkLogView", "avatar")
fn read_avatar(token: String, user: String) -> Result(BitArray, String)

@external(erlang, "eWorkLogView", "desktop_avatar")
fn read_desktop_avatar(
  tokens: List(String),
  user: String,
) -> Result(BitArray, String)

@external(erlang, "eTodoOidc", "enabled")
fn oidc_enabled() -> Bool

@external(erlang, "eTodoOidc", "start")
fn oidc_start() -> #(String, String)

@external(erlang, "eTodoOidc", "finish")
fn oidc_finish(
  code: String,
  state: String,
  cookie: String,
) -> Result(String, String)

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

@external(erlang, "eTodoMistAuth", "set_reminder")
fn set_reminder(
  token: String,
  csrf: String,
  uid: String,
  enabled: Bool,
  date: String,
  time: String,
  recurrence: String,
  end_date: String,
) -> Result(Nil, Dynamic)

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
  lists: List(String),
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

@external(erlang, "eTodoMistAuth", "create_list")
fn create_list(
  token: String,
  csrf: String,
  name: String,
) -> Result(String, Dynamic)

@external(erlang, "eTodoMistAuth", "delete_list")
fn delete_list(
  token: String,
  csrf: String,
  name: String,
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

@external(erlang, "eTodoMistAuth", "move_task")
fn move_task(
  token: String,
  csrf: String,
  list_name: String,
  uid: String,
  before_uid: String,
) -> Result(Nil, Dynamic)

@external(erlang, "eTodoMist", "read_bootstrap_stylesheet")
fn read_bootstrap_stylesheet() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_stylesheet")
fn read_etodo_stylesheet() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_script")
fn read_etodo_script() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_logo")
fn read_etodo_logo() -> Result(BitArray, String)

@external(erlang, "eTodoMist", "read_etodo_icon")
fn read_etodo_icon(file: String) -> Result(BitArray, String)

@external(erlang, "eTodoMistAuth", "status_form")
fn status_form(token: String) -> Result(#(String, Bool), String)

@external(erlang, "eTodoMistAuth", "set_status")
fn set_status(
  token: String,
  csrf: String,
  status: String,
  message: String,
) -> Result(Nil, Dynamic)

@external(erlang, "eTodoMistAuth", "set_team_pomodoro")
fn set_team_pomodoro(
  token: String,
  csrf: String,
  enabled: Bool,
) -> Result(Nil, Dynamic)

@external(erlang, "eTodoMist", "status_json")
fn status_json(user: String) -> Result(String, String)

@external(erlang, "eTodoMist", "status_share_key")
fn status_share_key() -> String

@external(erlang, "eTodoMist", "public_status_json")
fn public_status_json(key: String) -> Result(String, String)

@external(erlang, "eTodoMist", "read_linked_file")
fn read_linked_file(
  reference: String,
  file_name: String,
) -> Result(#(BitArray, String, String), String)

pub fn start(port: Int, host: String, certfile: String, keyfile: String) {
  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      use <- signed_out_guard(request)
      case request.method, request.path_segments(request) {
        Get, ["assets", "bootstrap.min.css"] -> bootstrap_response()
        Get, ["assets", "etodo.css"] ->
          stylesheet_response(read_etodo_stylesheet())
        Get, ["assets", "etodo.js"] -> script_response(read_etodo_script())
        Get, ["assets", "etodo-logo.png"] -> logo_response(read_etodo_logo())
        Get, ["assets", "icon-192.png"] ->
          logo_response(read_etodo_icon("etodoIcon192.png"))
        Get, ["assets", "icon-512.png"] ->
          logo_response(read_etodo_icon("etodoIcon512.png"))
        Get, ["assets", "icon-maskable-512.png"] ->
          logo_response(read_etodo_icon("etodoMaskable512.png"))
        Get, ["assets", "apple-touch-icon.png"] ->
          logo_response(read_etodo_icon("etodoApple180.png"))
        Get, ["assets", "status-available.png"] ->
          logo_response(read_etodo_icon("status-available.png"))
        Get, ["assets", "status-busy.png"] ->
          logo_response(read_etodo_icon("status-busy.png"))
        Get, ["manifest.json"] -> manifest_response()
        Get, ["file"] -> serve_linked_file(request)
        Get, ["status"] -> serve_status(request)
        Get, ["status.json"] -> serve_status_json(request)
        Get, ["status", "view", key] -> serve_public_status(key)
        Get, ["status", "view", key, "json"] -> serve_public_status_json(key)
        // Task links in messages and reports, written for the desktop client.
        Get, ["work-log", "action", "task", uid]
        | Get, ["work-log", "action", "time", uid, _, _, _]
        -> open_task(uid)
        Get, ["work-log"] -> serve_report(request, "work-log")
        Get, ["time-log"] -> serve_report(request, "time-log")
        Get, ["schedule"] -> serve_report(request, "schedule")
        Get, ["messages"] -> serve_messages(request)
        Get, ["messages.json"] -> serve_messages_json(request)
        Get, ["events"] -> serve_events(request)
        Get, ["todo", "order"] -> serve_todo_order(request)
        Post, ["messages", "send"] -> send_message_request(request)
        Post, ["todo", "markdown-preview"] -> todo_markdown_preview(request)
        Get, ["avatar"] -> serve_avatar(request)
        Post, ["status"] -> save_status_request(request)
        Post, ["status", "pomodoro"] -> team_pomodoro_request(request)
        Get, [] -> serve_home(request)
        Get, ["settings"] -> serve_settings(request)
        Post, ["settings"] -> save_settings_request(request)
        Get, ["auth", "google"] -> google_start(request)
        Get, ["auth", "google", "callback"] -> google_callback(request)
        Post, ["login"] -> login(request)
        Post, ["logout"] -> logout_request(request)
        Post, ["todo", "done"] -> todo_done(request)
        Post, ["todo", "move"] -> todo_move(request)
        Post, ["todo", "update"] -> todo_update(request)
        Post, ["todo", "delete"] -> todo_delete(request)
        Post, ["todo", "create"] -> todo_create(request)
        Post, ["list", "create"] -> list_create(request)
        Post, ["list", "delete"] -> list_delete(request)
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

pub fn start_work_log(owner: process.Pid) {
  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      let host = request.get_header(request, "host") |> result.unwrap("")
      case desktop_host_allowed(host) {
        False -> response(403, "Forbidden")
        True ->
          case request.method, request.path_segments(request) {
            Get, ["assets", "bootstrap.min.css"] -> bootstrap_response()
            Get, ["assets", "etodo.js"] -> script_response(read_etodo_script())
            Get, ["desktop", ticket] ->
              desktop_bootstrap(request, "work-log", ticket)
            Get, ["desktop", report, ticket] ->
              desktop_bootstrap(request, report, ticket)
            Get, ["work-log"] -> desktop_report(request, "work-log")
            Get, ["time-log"] -> desktop_report(request, "time-log")
            Get, ["schedule"] -> desktop_report(request, "schedule")
            Get, ["messages"] -> desktop_messages(request)
            Get, ["messages", "fragment"] -> desktop_fragment(request)
            Post, ["messages", action] -> desktop_action(request, action)
            Get, ["events"] -> desktop_events(request)
            Get, ["avatar"] ->
              logo_response(read_desktop_avatar(
                cookie_values(request, instance_cookie(request, desktop_cookie)),
                query_value(request, "user"),
              ))
            _, _ -> response(404, "Not found")
          }
      }
    }
    |> mist.new
    |> mist.bind("127.0.0.1")
    |> mist.port(0)
    |> mist.after_start(fn(port, _, _) { desktop_listener_started(owner, port) })
    |> mist.start

  process.sleep_forever()
}

fn desktop_bootstrap(
  request: Request(Connection),
  report: String,
  ticket: String,
) -> Response(ResponseData) {
  case desktop_bootstrap_token(ticket) {
    Ok(token) -> {
      let query =
        [
          "date", "list", "kind", "filter", "peers", "background", "foreground",
          "accent",
        ]
        |> list.map(fn(name) {
          name <> "=" <> uri.percent_encode(query_value(request, name))
        })
        |> string.join("&")
      // Messages open scrolled to the newest entry.
      let fragment = case report {
        "messages" -> "#end"
        _ -> ""
      }
      response.new(303)
      |> response.set_header(
        "location",
        "/" <> report <> "?" <> query <> fragment,
      )
      |> response.set_header("cache-control", "no-store")
      |> response.set_header("referrer-policy", "no-referrer")
      |> response.prepend_header(
        "set-cookie",
        desktop_cookie_header(
          instance_cookie(request, desktop_cookie),
          token,
          None,
          "/",
        ),
      )
      |> response.prepend_header(
        "set-cookie",
        desktop_cookie_header(
          instance_cookie(request, legacy_desktop_cookie),
          "",
          Some(0),
          "/",
        ),
      )
      |> response.prepend_header(
        "set-cookie",
        desktop_cookie_header(
          instance_cookie(request, legacy_desktop_cookie),
          "",
          Some(0),
          "/work-log",
        ),
      )
      |> response_data
    }
    Error(_) -> response(401, "Desktop sign-in expired")
  }
}

fn desktop_cookie_header(
  name: String,
  value: String,
  max_age: Option(Int),
  path: String,
) -> String {
  cookie.set_header(
    name,
    value,
    cookie.Attributes(
      max_age: max_age,
      domain: None,
      path: Some(path),
      secure: False,
      http_only: True,
      same_site: Some(cookie.Strict),
    ),
  )
}

fn desktop_report(
  request: Request(Connection),
  report: String,
) -> Response(ResponseData) {
  let render = fn(token) {
    desktop_report_page(
      token,
      report,
      query_value(request, "date"),
      query_value(request, "list"),
      query_value(request, "background"),
      query_value(request, "foreground"),
      query_value(request, "accent"),
    )
  }
  case
    first_valid_desktop_page(
      cookie_values(request, instance_cookie(request, desktop_cookie)),
      render,
    )
  {
    Ok(page) -> work_log_response(page)
    Error("invalid_session") -> response(401, "Desktop sign-in expired")
    Error(_) -> response(400, "Could not load report")
  }
}

fn desktop_messages(request: Request(Connection)) -> Response(ResponseData) {
  let render = fn(token) {
    desktop_messages_page(
      token,
      query_value(request, "kind"),
      query_value(request, "filter"),
      query_value(request, "peers"),
      query_value(request, "background"),
      query_value(request, "foreground"),
      query_value(request, "accent"),
    )
  }
  case
    first_valid_desktop_page(
      cookie_values(request, instance_cookie(request, desktop_cookie)),
      render,
    )
  {
    Ok(page) -> messages_response(page)
    Error("invalid_session") -> response(401, "Desktop sign-in expired")
    Error(_) -> response(400, "Could not load messages")
  }
}

fn desktop_fragment(request: Request(Connection)) -> Response(ResponseData) {
  let render = fn(token) {
    desktop_messages_fragment(
      token,
      query_value(request, "filter"),
      query_value(request, "peers"),
    )
  }
  case
    first_valid_desktop_page(
      cookie_values(request, instance_cookie(request, desktop_cookie)),
      render,
    )
  {
    Ok(body) ->
      response(200, body)
      |> response.set_header("content-type", "application/json")
      |> response.set_header("cache-control", "no-store")
    Error("invalid_session") -> response(401, "Desktop sign-in expired")
    Error(_) -> response(500, "Could not read messages")
  }
}

// Composer actions of the desktop Messages view: send, preview, typing,
// linkfile and viewing.
fn desktop_action(
  request: Request(Connection),
  action: String,
) -> Response(ResponseData) {
  case form_fields(request) {
    Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      let run = fn(token) {
        desktop_message_action(token, field("csrf"), action, field("value"))
      }
      case
        first_valid_desktop_page(
          cookie_values(request, instance_cookie(request, desktop_cookie)),
          run,
        )
      {
        Ok(body) ->
          response(200, body)
          |> response.set_header("content-type", "text/html; charset=utf-8")
          |> response.set_header("cache-control", "no-store")
        Error("invalid_session") -> response(401, "Desktop sign-in expired")
        Error(reason) -> response(400, reason)
      }
    }
    Error(_) -> response(400, "Invalid request")
  }
}

fn desktop_events(request: Request(Connection)) -> Response(ResponseData) {
  case
    first_desktop_user(cookie_values(
      request,
      instance_cookie(request, desktop_cookie),
    ))
  {
    Ok(username) -> event_stream(request, username)
    Error(_) -> response(401, "Desktop sign-in expired")
  }
}

fn first_desktop_user(tokens: List(String)) -> Result(String, Nil) {
  case tokens {
    [] -> Error(Nil)
    [token, ..rest] ->
      case desktop_user(token) {
        Ok(username) -> Ok(username)
        Error(_) -> first_desktop_user(rest)
      }
  }
}

fn serve_messages(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(token, _)) ->
      case messages_page(token, query_value(request, "kind")) {
        Ok(page) -> messages_response(page)
        Error(_) -> response(400, "Could not load messages")
      }
    Error(_) -> {
      let kind = case query_value(request, "kind") {
        "reminders" -> "reminders"
        "system" -> "system"
        _ -> "chat"
      }
      login_page("")
      |> response.prepend_header(
        "set-cookie",
        messages_cookie_header(request, kind, 600),
      )
    }
  }
}

fn messages_cookie_header(
  request: Request(Connection),
  kind: String,
  max_age: Int,
) -> String {
  cookie.set_header(
    instance_cookie(request, messages_cookie),
    kind,
    cookie.Attributes(
      max_age: Some(max_age),
      domain: None,
      path: Some("/"),
      secure: True,
      http_only: True,
      same_site: Some(cookie.Lax),
    ),
  )
}

fn messages_destination(request: Request(Connection)) -> String {
  // Only known local views are accepted, never a URL supplied by a cookie.
  case cookie_value(request, instance_cookie(request, messages_cookie)) {
    Some("chat") -> "/messages?kind=chat"
    Some("reminders") -> "/messages?kind=reminders"
    Some("system") -> "/messages?kind=system"
    _ -> "/"
  }
}

fn messages_response(page: String) -> Response(ResponseData) {
  work_log_response(page)
  |> response.set_header(
    "content-security-policy",
    "default-src 'none'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; script-src 'self'; connect-src 'self'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'",
  )
}

fn serve_avatar(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(token, _)) ->
      logo_response(read_avatar(token, query_value(request, "user")))
    Error(_) -> response(401, "Not signed in")
  }
}

fn first_valid_desktop_page(
  tokens: List(String),
  render: fn(String) -> Result(String, String),
) -> Result(String, String) {
  case tokens {
    [] -> Error("invalid_session")
    [token, ..rest] ->
      case render(token) {
        Error("invalid_session") -> first_valid_desktop_page(rest, render)
        result -> result
      }
  }
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
                Ok(page) -> remember_list(request, page, "All tasks")
                Error(message) -> page_error(message)
              }
            Ok(page) -> remember_list(request, page, list_name)
            Error(message) -> page_error(message)
          }
        }
        Error(_) -> response(400, "Invalid list selection")
      }
    Error(_) -> login_page("")
  }
}

fn serve_todo_order(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request), requested_list(request) {
    Ok(#(token, _)), Ok(#(list_name, _)) ->
      case todo_data(token, list_name, wants_all(request)) {
        Ok(encoded) ->
          case json.parse(encoded, todo_page_data_decoder()) {
            Ok(data) ->
              response(
                200,
                json.array(data.todos, fn(task) { json.string(task.uid) })
                  |> json.to_string,
              )
              |> response.set_header("content-type", "application/json")
            Error(_) -> response(500, "Could not decode todo data")
          }
        Error("invalid_session") -> response(401, "Not signed in")
        Error("invalid_list") -> response(400, "Unknown todo list")
        Error(_) -> response(500, "Could not load todo order")
      }
    Error(_), _ -> response(401, "Not signed in")
    _, Error(_) -> response(400, "Invalid list selection")
  }
}

fn serve_report(
  request: Request(Connection),
  report: String,
) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(token, _)) -> {
      // Without an explicit choice, follow the list last used on the task page.
      let list_name = case requested_list(request) {
        Ok(#(name, _)) -> name
        Error(_) -> "All tasks"
      }
      case report_page(token, report, query_value(request, "date"), list_name) {
        // Same policy as Messages: the page script listens for pushes.
        Ok(page) -> messages_response(page)
        Error(_) -> response(400, "Could not load report")
      }
    }
    Error(_) -> login_page("")
  }
}

fn work_log_response(page: String) -> Response(ResponseData) {
  response(200, page)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_header("cache-control", "no-store")
  |> response.set_header("referrer-policy", "no-referrer")
  |> response.set_header(
    "content-security-policy",
    "default-src 'none'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; script-src 'none'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'",
  )
}

// One Server-Sent Events stream per open tab; eTodoPush feeds it.
fn serve_events(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(_, username)) -> event_stream(request, username)
    Error(_) -> response(401, "Not signed in")
  }
}

fn event_stream(
  request: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  mist.server_sent_events(
    request,
    response.new(200)
      |> response.set_header("cache-control", "no-store")
      |> response.set_header("x-accel-buffering", "no"),
    init: fn(subject) { push_subscribe(username, subject) },
    loop: fn(state, event, connection) {
      let message =
        mist.event(string_tree.from_string(event))
        |> mist.event_name(case event {
          "ping" -> "ping"
          "reports" -> "reports"
          "status" -> "status"
          "counts" -> "counts"
          "recipients" -> "recipients"
          "todo_order" -> "todo_order"
          _ -> "messages"
        })
      case mist.send_event(connection, message) {
        Ok(Nil) -> actor.continue(state)
        Error(Nil) -> actor.stop()
      }
    },
  )
}

fn serve_messages_json(request: Request(Connection)) -> Response(ResponseData) {
  case current_session(request) {
    Ok(#(token, _)) ->
      case messages_summary(token, query_value(request, "since")) {
        Ok(body) ->
          response(200, body)
          |> response.set_header("content-type", "application/json")
          |> response.set_header("cache-control", "no-store")
        Error(_) -> response(500, "Could not read messages")
      }
    Error(_) -> response(401, "Not signed in")
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
        Ok(#(csrf, can_edit)) ->
          status_page(
            "/status.json",
            html.footer([attribute.id("status-controls")], [
              html.a([attribute.href("/"), attribute.id("status-back")], [
                html.text("Back to todos"),
              ]),
              case can_edit {
                True ->
                  html.details([attribute.id("status-edit")], [
                    html.summary([], [html.text("Change status")]),
                    status_form_element(csrf, status_share_key()),
                  ])
                False -> element.none()
              },
            ]),
          )
        Error(_) -> login_page("Session expired. Sign in again.")
      }
    Error(_) -> login_page("")
  }
}

// The read-only status page for family and colleagues: no login and no
// controls. The key in the link is the only credential.
fn serve_public_status(key: String) -> Response(ResponseData) {
  case public_status_json(key) {
    Ok(_) ->
      status_page("/status/view/" <> key <> "/json", element.none())
      |> response.set_header("referrer-policy", "no-referrer")
    Error(_) -> response(404, "Not found")
  }
}

fn serve_public_status_json(key: String) -> Response(ResponseData) {
  case public_status_json(key) {
    Ok(body) ->
      response(200, body)
      |> response.set_header("content-type", "application/json")
      |> response.set_header("cache-control", "no-store")
    Error(_) -> response(404, "Not found")
  }
}

fn send_message_request(
  request: Request(Connection),
) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      case send_message(token, field("csrf"), field("to"), field("msg")) {
        Ok(Nil) ->
          response.new(303)
          |> response.set_header("location", "/messages#chat")
          |> response_data
        Error(_) -> response(400, "Could not send the message")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn todo_markdown_preview(
  request: Request(Connection),
) -> Response(ResponseData) {
  case current_session(request), form_fields(request) {
    Ok(_), Ok(fields) -> {
      let text = list.key_find(fields, "text") |> result.unwrap("")
      case string.length(text) > 30_000 {
        True -> response(413, "Markdown preview is too large")
        False ->
          response(200, markdown_html(text))
          |> response.set_header("content-type", "text/html; charset=utf-8")
          |> response.set_header("cache-control", "no-store")
      }
    }
    Error(_), _ -> response(401, "Not signed in")
    _, Error(_) -> response(400, "Invalid request")
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

fn team_pomodoro_request(
  request: Request(Connection),
) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      case set_team_pomodoro(token, field("csrf"), field("enabled") == "true") {
        Ok(Nil) ->
          case request.get_header(request, "x-etodo-fetch") {
            Ok(_) -> response(200, "ok")
            Error(_) ->
              response.new(303)
              |> response.set_header("location", "/status")
              |> response_data
          }
        Error(_) -> response(400, "Could not change the team pomodoro")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn status_page(
  json_url: String,
  footer: element.Element(Nil),
) -> Response(ResponseData) {
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
        // Without the app manifest, "Add to Home Screen" keeps this page's
        // address instead of opening the todo app.
        case footer == element.none() {
          True -> element.none()
          False ->
            html.link([
              attribute.rel("manifest"),
              attribute.href("/manifest.json"),
            ])
        },
        html.link([
          attribute.rel("icon"),
          attribute.href("/assets/icon-192.png"),
        ]),
        html.link([
          attribute.rel("apple-touch-icon"),
          attribute.href("/assets/apple-touch-icon.png"),
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
        html.main(
          [attribute.id("status-page"), attribute.data("url", json_url)],
          [
            cell("status-user", ""),
            cell("status-state", ""),
            cell("status-msg", ""),
            html.div([attribute.id("status-break")], [
              html.text("\u{2615} Break"),
            ]),
            cell("status-timer", ""),
            cell("status-timer-desc", ""),
          ],
        ),
        footer,
      ]),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(document))
  |> secure_headers
}

fn status_form_element(csrf: String, share_key: String) -> element.Element(a) {
  let csrf_input =
    html.input([
      attribute.type_("hidden"),
      attribute.name("csrf"),
      attribute.value(csrf),
    ])
  html.div([attribute.id("status-panel")], [
    html.form(
      [
        attribute.id("status-form"),
        attribute.attribute("action", "/status"),
        attribute.attribute("method", "post"),
      ],
      [
        csrf_input,
        html.div(
          [
            attribute.attribute("class", "etodo-status-choices"),
            attribute.attribute("role", "radiogroup"),
            attribute.attribute("aria-label", "Status"),
          ],
          list.flat_map(["Available", "Busy", "Away"], fn(name) {
            let id = "status-choice-" <> string.lowercase(name)
            let icon = case name {
              "Available" -> "🟢"
              "Busy" -> "🔴"
              _ -> "🌴"
            }
            [
              html.input([
                attribute.type_("radio"),
                attribute.name("status"),
                attribute.id(id),
                attribute.value(name),
              ]),
              html.label(
                [
                  attribute.for(id),
                  attribute.attribute(
                    "class",
                    "etodo-status-choice-" <> string.lowercase(name),
                  ),
                ],
                [
                  html.span(
                    [
                      attribute.attribute("class", "etodo-status-choice-icon"),
                      attribute.attribute("aria-hidden", "true"),
                    ],
                    [html.text(icon)],
                  ),
                  html.text(" " <> name),
                ],
              ),
            ]
          }),
        ),
        html.input([
          attribute.type_("text"),
          attribute.name("message"),
          attribute.id("status-message"),
          attribute.attribute("maxlength", "200"),
          attribute.attribute("placeholder", "Status message"),
          attribute.attribute("aria-label", "Status message"),
          attribute.attribute("autocomplete", "off"),
        ]),
        html.button([attribute.type_("submit")], [html.text("Set status")]),
      ],
    ),
    // Turning the switch on or off takes effect right away.
    html.form(
      [
        attribute.id("pomodoro-form"),
        attribute.attribute("action", "/status/pomodoro"),
        attribute.attribute("method", "post"),
      ],
      [
        csrf_input,
        html.label([attribute.attribute("class", "etodo-switch")], [
          html.input([
            attribute.type_("checkbox"),
            attribute.name("enabled"),
            attribute.id("pomodoro-toggle"),
            attribute.value("true"),
            attribute.attribute("role", "switch"),
          ]),
          html.span([], [
            html.strong([], [html.text("Team pomodoro")]),
            html.small([], [
              html.text("25 min work, 5 min break, on the hour and half hour"),
            ]),
          ]),
        ]),
      ],
    ),
    html.div([attribute.id("status-share")], [
      html.strong([], [html.text("Read-only link")]),
      html.small([], [
        html.text("Shows your status without login or buttons."),
      ]),
      html.div([attribute.attribute("class", "etodo-share-row")], [
        html.a(
          [
            attribute.id("status-share-link"),
            attribute.href("/status/view/" <> share_key),
            attribute.target("_blank"),
            attribute.rel("noopener"),
          ],
          [html.text("Open")],
        ),
        html.button(
          [attribute.type_("button"), attribute.id("status-share-copy")],
          [html.text("Copy link")],
        ),
      ]),
    ]),
  ])
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
  request: Request(Connection),
  page: Response(ResponseData),
  list_name: String,
) -> Response(ResponseData) {
  response.prepend_header(
    page,
    "set-cookie",
    cookie.set_header(
      instance_cookie(request, list_cookie),
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

fn oidc_cookie_header(
  request: Request(Connection),
  value: String,
  max_age: Int,
) -> String {
  // Lax, not Strict: the cookie has to come back on the redirect from Google.
  cookie.set_header(
    instance_cookie(request, oidc_cookie),
    value,
    cookie.Attributes(
      max_age: Some(max_age),
      domain: None,
      path: Some("/auth"),
      secure: True,
      http_only: True,
      same_site: Some(cookie.Lax),
    ),
  )
}

fn google_start(request: Request(Connection)) -> Response(ResponseData) {
  case oidc_enabled() {
    False -> response(404, "Not found")
    True -> {
      let #(url, cookie_value) = oidc_start()
      response.new(303)
      |> response.set_header("location", url)
      |> response.prepend_header(
        "set-cookie",
        oidc_cookie_header(request, cookie_value, 600),
      )
      |> response_data
    }
  }
}

fn continue_page(destination: String) -> Response(ResponseData) {
  let body =
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"color-scheme\" content=\"light dark\"><meta http-equiv=\"refresh\" content=\"0;url="
    <> destination
    <> "\"><title>eTodo</title></head><body><p><a href=\""
    <> destination
    <> "\">Continue to eTodo</a></p></body></html>"
  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_header("cache-control", "no-store")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
  |> secure_headers
}

fn google_callback(request: Request(Connection)) -> Response(ResponseData) {
  let query = request.get_query(request) |> result.unwrap([])
  let cookies =
    request
    |> request.get_header("cookie")
    |> result.unwrap("")
    |> cookie.parse
  case
    oidc_enabled(),
    list.key_find(query, "code"),
    list.key_find(query, "state"),
    list.key_find(cookies, instance_cookie(request, oidc_cookie))
  {
    False, _, _, _ -> response(404, "Not found")
    True, Ok(code), Ok(state), Ok(cookie_value) ->
      case oidc_finish(code, state, cookie_value) {
        Ok(token) ->
          // A redirect here would count as cross-site (it started at Google),
          // so the browser would withhold the Strict session cookie. A page
          // that navigates itself to the destination is same-site.
          continue_page(messages_destination(request))
          |> response.prepend_header(
            "set-cookie",
            messages_cookie_header(request, "", 0),
          )
          |> response.prepend_header(
            "set-cookie",
            oidc_cookie_header(request, "", 0),
          )
          |> response.prepend_header(
            "set-cookie",
            cookie.set_header(
              instance_cookie(request, session_cookie),
              token,
              cookie.Attributes(
                max_age: Some(session_max_age),
                domain: None,
                path: Some("/"),
                secure: True,
                http_only: True,
                same_site: Some(cookie.Lax),
              ),
            ),
          )
        Error(message) ->
          login_page(message)
          |> response.prepend_header(
            "set-cookie",
            oidc_cookie_header(request, "", 0),
          )
      }
    True, _, _, _ ->
      login_page("Google sign-in was cancelled or failed.")
      |> response.prepend_header(
        "set-cookie",
        oidc_cookie_header(request, "", 0),
      )
  }
}

fn login(request: Request(Connection)) -> Response(ResponseData) {
  case form_values(request) {
    Ok(#(username, password)) ->
      case authenticate(username, password) {
        Ok(token) ->
          response.new(303)
          |> response.set_header("location", messages_destination(request))
          |> response.prepend_header(
            "set-cookie",
            messages_cookie_header(request, "", 0),
          )
          |> response.prepend_header(
            "set-cookie",
            cookie.set_header(
              instance_cookie(request, session_cookie),
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
      instance_cookie(request, session_cookie),
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
        Ok(length) if length >= 0 && length <= 16_384 -> Ok(Nil)
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
    |> mist.read_body(max_body_limit: 16_384)
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

fn todo_move(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      let list_name = case field("list") {
        "" -> "All tasks"
        name -> name
      }
      let uid = field("uid")
      case
        move_task(token, field("csrf"), list_name, uid, field("before_uid"))
      {
        Ok(Nil) ->
          case request.get_header(request, "x-etodo-fetch") {
            Ok(_) -> response(200, "")
            Error(_) -> {
              let all = case field("show_all") {
                "true" -> "&all=1"
                _ -> ""
              }
              response.new(303)
              |> response.set_header(
                "location",
                "/?list="
                  <> uri.percent_encode(list_name)
                  <> all
                  <> "#task-"
                  <> uid,
              )
              |> response_data
            }
          }
        Error(_) -> response(400, "Could not reorder the task")
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
          list.filter_map(fields, fn(pair) {
            case pair {
              #("lists", name) -> Ok(name)
              _ -> Error(Nil)
            }
          }),
        )
      {
        Ok(Nil) ->
          case
            set_reminder(
              token,
              field("csrf"),
              uid,
              field("reminder") == "on",
              field("reminder_date"),
              field("reminder_time"),
              field("reminder_recurrence"),
              field("reminder_end_date"),
            )
          {
            Ok(Nil) -> redirect_to_task(list_name, uid)
            Error(_) ->
              response(
                400,
                "The task was saved, but not the reminder: it needs a date",
              )
          }
        Error(_) -> response(400, "Could not update the task")
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn list_create(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      case create_list(token, field("csrf"), field("name")) {
        Ok(name) ->
          response.new(303)
          |> response.set_header(
            "location",
            "/?list=" <> uri.percent_encode(name),
          )
          |> response_data
        Error(_) ->
          response(
            400,
            "Could not create the list. Use a name of at most 100 characters, without < or >, that is not a number.",
          )
      }
    }
    _, _ -> response(400, "Invalid request")
  }
}

fn list_delete(request: Request(Connection)) -> Response(ResponseData) {
  case session_cookie_value(request), form_fields(request) {
    Some(token), Ok(fields) -> {
      let field = fn(name) { list.key_find(fields, name) |> result.unwrap("") }
      case field("confirm") {
        "yes" ->
          case delete_list(token, field("csrf"), field("name")) {
            Ok(_) ->
              response.new(303)
              |> response.set_header("location", "/?list=All%20tasks")
              |> response_data
            Error(_) -> response(400, "Could not delete the list")
          }
        // Not confirmed: back to the list, nothing deleted.
        _ ->
          response.new(303)
          |> response.set_header(
            "location",
            "/?list=" <> uri.percent_encode(field("name")),
          )
          |> response_data
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
        Ok(uid) ->
          case request.get_header(request, "x-etodo-fetch") {
            Ok(_) -> response(200, created_item_html(token, list_name, uid))
            Error(_) -> redirect_to_task(list_name, uid)
          }
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

/// Shows the task in "All tasks", including tasks the filter would hide.
fn open_task(uid: String) -> Response(ResponseData) {
  case int.parse(uid) {
    Ok(_) ->
      response.new(303)
      |> response.set_header(
        "location",
        "/?list=All%20tasks&all=1#task-" <> uid,
      )
      |> response_data
    Error(_) -> response(404, "Not found")
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

/// The list item of a task created from the quick-add bar, so the page can
/// show it without reloading. Empty if the task cannot be found.
fn created_item_html(token: String, list_name: String, uid: String) -> String {
  let marker_prefix = new_markdown_token()
  let item = {
    use encoded <- result.try(todo_data(token, list_name, True))
    use data <- result.try(
      json.parse(encoded, todo_page_data_decoder())
      |> result.replace_error("decode"),
    )
    use task <- result.map(
      list.find(data.todos, fn(task) { task.uid == uid })
      |> result.replace_error("missing"),
    )
    todo_item(task, data, marker_prefix)
    |> element.to_string
    |> expand_task_markdown([task], marker_prefix)
  }
  result.unwrap(item, "")
}

/// After a restart of eTodo all sessions are gone. A form posted from a page
/// opened before that goes back to the login page instead of failing, and a
/// request from the page script gets 401 so the script can do the same.
fn signed_out_guard(
  request: Request(Connection),
  next: fn() -> Response(ResponseData),
) -> Response(ResponseData) {
  let open = case request.path_segments(request) {
    ["login"] | ["logout"] -> True
    _ -> False
  }
  case request.method, open {
    Post, False ->
      case current_session(request) {
        Ok(_) -> next()
        Error(Nil) ->
          case request.get_header(request, "x-etodo-fetch") {
            Ok("1") -> response(401, "Not signed in")
            _ ->
              response.new(303)
              |> response.set_header("location", "/")
              |> response_data
          }
      }
    _, _ -> next()
  }
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
  let remembered = case
    cookie_value(request, instance_cookie(request, list_cookie))
  {
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

fn cookie_values(request: Request(Connection), name: String) -> List(String) {
  request
  |> request.get_header("cookie")
  |> result.unwrap("")
  |> cookie.parse
  |> list.filter_map(fn(pair) {
    case pair {
      #(key, value) if key == name -> Ok(value)
      _ -> Error(Nil)
    }
  })
}

fn session_cookie_value(request: Request(Connection)) -> Option(String) {
  let cookies =
    request
    |> request.get_header("cookie")
    |> result.unwrap("")
    |> cookie.parse

  case list.key_find(cookies, instance_cookie(request, session_cookie)) {
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
    |> list.append(case oidc_enabled() {
      False -> []
      True -> [
        html.div(
          [attribute.attribute("class", "text-center text-body-secondary my-3")],
          [
            html.text("or"),
          ],
        ),
        html.a(
          [
            attribute.attribute(
              "class",
              "btn btn-outline-secondary btn-lg w-100",
            ),
            attribute.href("/auth/google"),
          ],
          [html.text("Sign in with Google")],
        ),
      ]
    })

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
          attribute.href("/assets/apple-touch-icon.png"),
        ]),
        html.meta([
          attribute.name("theme-color"),
          attribute.content("#f5f7fb"),
          attribute.attribute("media", "(prefers-color-scheme: light)"),
        ]),
        html.meta([
          attribute.name("theme-color"),
          attribute.content("#1b1e20"),
          attribute.attribute("media", "(prefers-color-scheme: dark)"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/bootstrap.min.css"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href("/assets/etodo.css"),
        ]),
        html.title([], "eTodo login"),
        html.script(
          [attribute.src("/assets/etodo.js"), attribute.attribute("defer", "")],
          "",
        ),
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

// Bootstrap is a vendored, versioned file; every page reload re-fetched it.
fn bootstrap_response() -> Response(ResponseData) {
  stylesheet_response(read_bootstrap_stylesheet())
  |> response.set_header("cache-control", "public, max-age=604800")
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
    "{\"name\":\"eTodo\",\"short_name\":\"eTodo\",\"start_url\":\"/\",\"display\":\"standalone\",\"background_color\":\"#ffffff\",\"theme_color\":\"#ffffff\",\"icons\":[{\"src\":\"/assets/icon-192.png\",\"sizes\":\"192x192\",\"type\":\"image/png\"},{\"src\":\"/assets/icon-512.png\",\"sizes\":\"512x512\",\"type\":\"image/png\"},{\"src\":\"/assets/icon-maskable-512.png\",\"sizes\":\"512x512\",\"type\":\"image/png\",\"purpose\":\"maskable\"}]}"
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

const header_button_class =
  "btn btn-outline-primary btn-lg text-nowrap etodo-header-button"

// Narrow screens show only the icon; the label returns from the md breakpoint.
fn header_button_content(icon: String, label: String) {
  [
    html.span([attribute.attribute("aria-hidden", "true")], [html.text(icon)]),
    html.span([attribute.attribute("class", "d-none d-md-inline ms-1")], [
      html.text(label),
    ]),
  ]
}

fn authenticated_page(
  username: String,
  data: TodoPageData,
) -> Response(ResponseData) {
  let can_delete = data.can_edit && data.selected_list != "All tasks"
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

  let marker_prefix = new_markdown_token()
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
    todos -> todo_items(todos, data, marker_prefix)
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
          attribute.href("/assets/apple-touch-icon.png"),
        ]),
        html.meta([
          attribute.name("theme-color"),
          attribute.content("#f5f7fb"),
          attribute.attribute("media", "(prefers-color-scheme: light)"),
        ]),
        html.meta([
          attribute.name("theme-color"),
          attribute.content("#1b1e20"),
          attribute.attribute("media", "(prefers-color-scheme: dark)"),
        ]),
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
      html.body(
        [
          attribute.attribute("class", "etodo-page"),
          attribute.attribute("data-swipe-next", "/messages#chat"),
        ],
        [
          html.header([attribute.attribute("class", "bg-body border-bottom")], [
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
                      "ms-auto d-flex align-items-center gap-2",
                    ),
                  ],
                  [
                    html.span(
                      [
                        attribute.attribute(
                          "class",
                          "small text-body-secondary d-none d-lg-inline me-1",
                        ),
                      ],
                      [
                        html.text("Signed in as "),
                        html.strong([], [html.text(username)]),
                      ],
                    ),
                    html.a(
                      [
                        attribute.href("/messages#chat"),
                        attribute.attribute("class", header_button_class),
                        attribute.attribute("aria-label", "Messages"),
                        attribute.title("Messages"),
                      ],
                      header_button_content("💬", "Messages"),
                    ),
                    html.a(
                      [
                        attribute.href("/status"),
                        attribute.attribute("class", header_button_class),
                        attribute.attribute("aria-label", "Status"),
                        attribute.title("Status"),
                      ],
                      [
                        html.span(
                          [
                            attribute.id("status-icon"),
                            attribute.attribute("aria-hidden", "true"),
                          ],
                          [html.text("🟢")],
                        ),
                        html.span(
                          [
                            attribute.attribute(
                              "class",
                              "d-none d-md-inline ms-1",
                            ),
                          ],
                          [html.text("Status")],
                        ),
                      ],
                    ),
                    html.a(
                      [
                        attribute.href("/settings"),
                        attribute.attribute("class", header_button_class),
                        attribute.attribute("aria-label", "Settings"),
                        attribute.title("Settings"),
                      ],
                      header_button_content("⚙️", "Settings"),
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
                            attribute.attribute("class", header_button_class),
                            attribute.attribute("type", "submit"),
                            attribute.attribute("aria-label", "Sign out"),
                            attribute.title("Sign out"),
                          ],
                          header_button_content("🚪", "Sign out"),
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
                      case data.can_edit {
                        True ->
                          html.input([
                            attribute.attribute("type", "checkbox"),
                            attribute.attribute("id", "newlist-toggle"),
                            attribute.attribute("class", "etodo-newlist-check"),
                            attribute.attribute(
                              "aria-label",
                              "Show the new list form",
                            ),
                          ])
                        False -> element.none()
                      },
                      case can_delete {
                        True ->
                          html.input([
                            attribute.attribute("type", "checkbox"),
                            attribute.attribute("id", "dellist-toggle"),
                            attribute.attribute("class", "etodo-newlist-check"),
                            attribute.attribute(
                              "aria-label",
                              "Show the delete list form",
                            ),
                          ])
                        False -> element.none()
                      },
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
                                attribute.attribute(
                                  "aria-label",
                                  "Refresh list",
                                ),
                              ],
                              [html.text("↻")],
                            ),
                            case data.can_edit {
                              True ->
                                html.label(
                                  [
                                    attribute.attribute("for", "newlist-toggle"),
                                    attribute.attribute(
                                      "class",
                                      "btn btn-outline-primary btn-lg ms-2 etodo-newlist-button",
                                    ),
                                    attribute.title("New list"),
                                  ],
                                  [
                                    html.span(
                                      [
                                        attribute.attribute(
                                          "class",
                                          "etodo-newlist-open",
                                        ),
                                      ],
                                      [html.text("+")],
                                    ),
                                    html.span(
                                      [
                                        attribute.attribute(
                                          "class",
                                          "etodo-newlist-close",
                                        ),
                                      ],
                                      [html.text("×")],
                                    ),
                                  ],
                                )
                              False -> element.none()
                            },
                            case can_delete {
                              True ->
                                html.label(
                                  [
                                    attribute.attribute("for", "dellist-toggle"),
                                    attribute.attribute(
                                      "class",
                                      "btn btn-outline-danger btn-lg ms-2 etodo-dellist-button",
                                    ),
                                    attribute.title("Delete list"),
                                  ],
                                  [
                                    html.span(
                                      [
                                        attribute.attribute(
                                          "class",
                                          "etodo-dellist-open",
                                        ),
                                      ],
                                      [html.text("−")],
                                    ),
                                    html.span(
                                      [
                                        attribute.attribute(
                                          "class",
                                          "etodo-dellist-close",
                                        ),
                                      ],
                                      [html.text("×")],
                                    ),
                                  ],
                                )
                              False -> element.none()
                            },
                          ]),
                        ],
                      ),
                      case data.can_edit {
                        True ->
                          html.form(
                            [
                              attribute.attribute("action", "/list/create"),
                              attribute.attribute("method", "post"),
                              attribute.attribute(
                                "class",
                                "etodo-newlist-panel row g-2 mt-2",
                              ),
                            ],
                            [
                              html.input([
                                attribute.attribute("type", "hidden"),
                                attribute.name("csrf"),
                                attribute.attribute("value", data.csrf),
                              ]),
                              html.div([attribute.attribute("class", "col")], [
                                html.input([
                                  attribute.attribute(
                                    "class",
                                    "form-control form-control-lg",
                                  ),
                                  attribute.attribute("type", "text"),
                                  attribute.name("name"),
                                  attribute.attribute("maxlength", "100"),
                                  attribute.attribute("required", "required"),
                                  attribute.attribute(
                                    "placeholder",
                                    "New list name",
                                  ),
                                  attribute.attribute(
                                    "aria-label",
                                    "New list name",
                                  ),
                                  attribute.attribute("autocomplete", "off"),
                                ]),
                              ]),
                              html.div(
                                [attribute.attribute("class", "col-auto")],
                                [
                                  html.button(
                                    [
                                      attribute.attribute(
                                        "class",
                                        "btn btn-primary btn-lg",
                                      ),
                                      attribute.attribute("type", "submit"),
                                    ],
                                    [html.text("Add list")],
                                  ),
                                ],
                              ),
                            ],
                          )
                        False -> element.none()
                      },
                      case can_delete {
                        True ->
                          html.form(
                            [
                              attribute.attribute("action", "/list/delete"),
                              attribute.attribute("method", "post"),
                              attribute.attribute(
                                "class",
                                "etodo-dellist-panel row g-2 mt-2 align-items-center",
                              ),
                            ],
                            [
                              html.input([
                                attribute.attribute("type", "hidden"),
                                attribute.name("csrf"),
                                attribute.attribute("value", data.csrf),
                              ]),
                              html.input([
                                attribute.attribute("type", "hidden"),
                                attribute.name("name"),
                                attribute.attribute("value", data.selected_list),
                              ]),
                              html.div([attribute.attribute("class", "col")], [
                                html.text("Delete the list \""),
                                html.strong([], [html.text(data.selected_list)]),
                                html.text(
                                  "\"? Its tasks stay in \"All tasks\".",
                                ),
                              ]),
                              html.div(
                                [
                                  attribute.attribute(
                                    "class",
                                    "col-12 d-flex flex-wrap align-items-center gap-3",
                                  ),
                                ],
                                [
                                  html.input([
                                    attribute.attribute(
                                      "class",
                                      "form-check-input mt-0",
                                    ),
                                    attribute.attribute("type", "checkbox"),
                                    attribute.id("dellist-confirm"),
                                    attribute.name("confirm"),
                                    attribute.attribute("value", "yes"),
                                  ]),
                                  html.label(
                                    [
                                      attribute.attribute(
                                        "class",
                                        "form-check-label",
                                      ),
                                      attribute.attribute(
                                        "for",
                                        "dellist-confirm",
                                      ),
                                    ],
                                    [html.text("Yes, delete this list")],
                                  ),
                                  html.button(
                                    [
                                      attribute.attribute(
                                        "class",
                                        "btn btn-danger btn-lg etodo-dellist-submit",
                                      ),
                                      attribute.attribute("type", "submit"),
                                    ],
                                    [html.text("Delete list")],
                                  ),
                                ],
                              ),
                            ],
                          )
                        False -> element.none()
                      },
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
              reorder_hint(data),
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
                  attribute.attribute("data-list", data.selected_list),
                  attribute.attribute("data-show-all", case data.show_all {
                    True -> "true"
                    False -> "false"
                  }),
                  attribute.attribute(
                    "data-reorderable",
                    case data.can_reorder {
                      True -> "true"
                      False -> "false"
                    },
                  ),
                  attribute.attribute(
                    "class",
                    "list-unstyled d-grid gap-3 mb-0",
                  ),
                ],
                todo_items,
              ),
            ],
          ),
        ],
      ),
    ])
    |> element.to_document_string_tree
    |> string_tree.to_string
    |> expand_task_markdown(data.todos, marker_prefix)
    |> string_tree.from_string
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
          attribute.href("/assets/apple-touch-icon.png"),
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
        html.script(
          [attribute.src("/assets/etodo.js"), attribute.attribute("defer", "")],
          "",
        ),
      ]),
      html.body([attribute.attribute("class", "etodo-page")], [
        html.header([attribute.attribute("class", "bg-body border-bottom")], [
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

fn markdown_marker(prefix: String, uid: String, field: String) -> String {
  "ETODO-MARKDOWN-" <> prefix <> "-" <> uid <> "-" <> field
}

fn expand_task_markdown(
  document: String,
  todos: List(Todo),
  prefix: String,
) -> String {
  list.fold(todos, document, fn(output, task) {
    output
    |> string.replace(
      markdown_marker(prefix, task.uid, "TITLE"),
      markdown_inline_html(truncate_description(task.description)),
    )
    |> string.replace(
      markdown_marker(prefix, task.uid, "DESCRIPTION"),
      markdown_html(task.description),
    )
    |> string.replace(
      markdown_marker(prefix, task.uid, "COMMENT"),
      markdown_html(task.comment),
    )
  })
}

// Keep task rows to one line, but format the first description line as Markdown.
fn truncate_description(description: String) -> String {
  let text =
    description
    |> string.split("\n")
    |> list.first
    |> result.unwrap("")
    |> string.replace("\r", " ")
    |> string.split(" ")
    |> list.filter(fn(word) { word != "" })
    |> string.join(" ")
  case string.length(text) > max_title_length {
    True -> string.trim_end(string.slice(text, 0, max_title_length)) <> "..."
    False -> text
  }
}

fn todo_item(task: Todo, data: TodoPageData, prefix: String) {
  let first_uid = case data.todos {
    [first, ..] -> first.uid
    [] -> ""
  }
  todo_item_with_order(task, data, prefix, first_uid, "", "", False)
}

fn todo_item_with_order(
  task: Todo,
  data: TodoPageData,
  prefix: String,
  first_uid: String,
  previous_uid: String,
  down_before_uid: String,
  has_next: Bool,
) {
  let title_class = case task.done {
    True -> "etodo-title text-break etodo-done"
    False -> "etodo-title text-break"
  }
  let toggle_id = "details-" <> task.uid

  let title =
    html.div([attribute.attribute("class", title_class)], [
      html.text(markdown_marker(prefix, task.uid, "TITLE")),
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
    True -> [
      done_toggle(task, data),
      drag_handle(data),
      title,
      toggle_label,
    ]
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
      task_details(
        task,
        data,
        first_uid,
        previous_uid,
        down_before_uid,
        has_next,
        prefix,
      ),
    ],
  )
}

fn drag_handle(data: TodoPageData) {
  case data.can_reorder {
    True ->
      html.button(
        [
          attribute.attribute("type", "button"),
          attribute.attribute("class", "etodo-drag-handle"),
          attribute.attribute("aria-label", "Drag to reorder task"),
          attribute.title("Drag to reorder"),
        ],
        [html.text("⠿")],
      )
    False -> element.none()
  }
}

fn todo_items(todos: List(Todo), data: TodoPageData, prefix: String) {
  case todos {
    [] -> []
    [first, ..] -> todo_items_from(todos, data, prefix, first.uid, "")
  }
}

fn todo_items_from(
  todos: List(Todo),
  data: TodoPageData,
  prefix: String,
  first_uid: String,
  previous_uid: String,
) {
  case todos {
    [] -> []
    [task, ..rest] -> {
      let #(down_before_uid, has_next) = case rest {
        [] -> #("", False)
        [_, after_next, ..] -> #(after_next.uid, True)
        [_] -> #("", True)
      }
      [
        todo_item_with_order(
          task,
          data,
          prefix,
          first_uid,
          previous_uid,
          down_before_uid,
          has_next,
        ),
      ]
      |> list.append(todo_items_from(rest, data, prefix, first_uid, task.uid))
    }
  }
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

fn task_details(
  task: Todo,
  data: TodoPageData,
  first_uid: String,
  previous_uid: String,
  down_before_uid: String,
  has_next: Bool,
  prefix: String,
) {
  let description = case
    string.trim(task.description)
    == string.trim(truncate_description(task.description))
  {
    True -> element.none()
    False ->
      markdown_field(
        prefix,
        task.uid,
        "DESCRIPTION",
        "Description",
        task.description,
      )
  }
  let comment =
    markdown_field(prefix, task.uid, "COMMENT", "Comment", task.comment)
  let summary =
    html.div([attribute.attribute("class", "etodo-summary")], [
      status_chip(task.status),
      priority_chip(task.priority),
      due_date_chip(task.due_time),
      reminder_chip(task.reminder),
    ])
  let progress = progress_indicator(task.progress)
  let metadata =
    [
      lists_metadata(task.lists_db),
      time_metadata(task.estimate, task.remaining),
      owner_metadata(task.owner, task.shared_with),
      subtasks_metadata(task.has_sub_todos),
      done_time_metadata(task),
    ]
    |> list.flatten

  let metadata = case metadata {
    [] -> element.none()
    _ -> html.div([attribute.attribute("class", "etodo-meta-grid")], metadata)
  }
  let created = created_metadata(task.created)

  case data.can_edit {
    True ->
      html.div([attribute.attribute("class", "etodo-details")], [
        summary,
        progress,
        description,
        comment,
        metadata,
        created,
        html.div(
          [attribute.attribute("class", "etodo-actions mt-3 pt-3 border-top")],
          [
            move_controls(
              task,
              data,
              first_uid,
              previous_uid,
              down_before_uid,
              has_next,
            ),
            html.div(
              [
                attribute.attribute(
                  "class",
                  "etodo-edit-row position-relative mt-2 border-top",
                ),
              ],
              [
                html.details([], [
                  html.summary(
                    [
                      attribute.attribute(
                        "class",
                        "btn btn-outline-primary btn-sm",
                      ),
                    ],
                    [html.text("Edit task")],
                  ),
                  task_editor(task, data),
                ]),
                html.button(
                  [
                    attribute.attribute(
                      "class",
                      "btn btn-outline-danger btn-sm etodo-delete-task",
                    ),
                    attribute.attribute("type", "submit"),
                    attribute.attribute("form", "delete-" <> task.uid),
                  ],
                  [html.text("Delete task")],
                ),
              ],
            ),
          ],
        ),
      ])
    False ->
      html.div([attribute.attribute("class", "etodo-details")], [
        summary,
        progress,
        description,
        comment,
        metadata,
        created,
      ])
  }
}

fn markdown_field(
  prefix: String,
  uid: String,
  field: String,
  label: String,
  value: String,
) {
  case string.trim(value) == "" {
    True -> element.none()
    False ->
      html.section(
        [
          attribute.attribute("class", case field {
            "COMMENT" -> "etodo-markdown-field etodo-markdown-note"
            _ -> "etodo-markdown-field etodo-markdown-description"
          }),
          attribute.attribute("aria-label", label),
        ],
        [
          html.h3([attribute.attribute("class", "h6 fw-semibold")], [
            html.text(label),
          ]),
          html.div([attribute.attribute("class", "etodo-markdown-body")], [
            html.text(markdown_marker(prefix, uid, field)),
          ]),
        ],
      )
  }
}

fn reorder_hint(data: TodoPageData) {
  case data.can_edit && !data.can_reorder {
    False -> element.none()
    True -> {
      let message = case data.selected_list == "All tasks" {
        True -> "Choose a specific task list to reorder tasks by hand."
        False ->
          "Set both sort orders to Default in Settings to reorder manually."
      }
      html.p(
        [
          attribute.attribute(
            "class",
            "small text-body-secondary mb-3 etodo-reorder-hint",
          ),
        ],
        [html.text(message)],
      )
    }
  }
}

fn move_controls(
  task: Todo,
  data: TodoPageData,
  first_uid: String,
  previous_uid: String,
  down_before_uid: String,
  has_next: Bool,
) {
  case data.can_reorder {
    False -> element.none()
    True ->
      html.div(
        [
          attribute.attribute(
            "class",
            "etodo-move-controls d-flex flex-wrap gap-2",
          ),
          attribute.attribute("aria-label", "Reorder task"),
        ],
        [
          move_control(
            task,
            data,
            "top",
            first_uid,
            "Top",
            task.uid == first_uid,
          ),
          move_control(
            task,
            data,
            "up",
            previous_uid,
            "↑ Up",
            previous_uid == "",
          ),
          move_control(task, data, "down", down_before_uid, "↓ Down", !has_next),
          move_control(task, data, "bottom", "", "Bottom", !has_next),
        ],
      )
  }
}

fn move_control(
  task: Todo,
  data: TodoPageData,
  action: String,
  before_uid: String,
  label: String,
  disabled: Bool,
) {
  let hidden = fn(name, value) {
    html.input([
      attribute.attribute("type", "hidden"),
      attribute.name(name),
      attribute.attribute("value", value),
    ])
  }
  let show_all = case data.show_all {
    True -> "true"
    False -> "false"
  }
  let button_attrs = [
    attribute.attribute("type", "submit"),
    attribute.attribute("class", "btn btn-outline-secondary btn-sm"),
    attribute.attribute("data-move-action", action),
  ]
  let button_attrs = case disabled {
    True -> [attribute.attribute("disabled", "disabled"), ..button_attrs]
    False -> button_attrs
  }

  html.form(
    [
      attribute.attribute("action", "/todo/move"),
      attribute.attribute("method", "post"),
      attribute.attribute("class", "etodo-move-form"),
    ],
    [
      hidden("uid", task.uid),
      hidden("csrf", data.csrf),
      hidden("list", data.selected_list),
      hidden("before_uid", before_uid),
      hidden("show_all", show_all),
      html.button(button_attrs, [html.text(label)]),
    ],
  )
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

  html.div([attribute.attribute("class", "card border-0 shadow-sm mb-4")], [
    // The quick-add bar: type a description and press Enter (or +). The page
    // script posts it in the background, so the keyboard stays open on a
    // phone and several todos can be added one after another.
    html.form(
      [
        attribute.attribute("id", "quick-add"),
        attribute.attribute("action", "/todo/create"),
        attribute.attribute("method", "post"),
        attribute.attribute("class", "card-body p-3 pb-2"),
      ],
      [
        hidden("csrf", data.csrf),
        hidden("list", data.selected_list),
        hidden("status", "Planning"),
        hidden("priority", "N/A"),
        hidden("progress", "0"),
        hidden("estimate", "0"),
        hidden("remaining", "0"),
        html.div([attribute.attribute("class", "input-group input-group-lg")], [
          html.input([
            attribute.attribute("type", "text"),
            attribute.attribute("id", "quick-description"),
            attribute.name("description"),
            attribute.attribute("class", "form-control"),
            attribute.attribute("placeholder", "Add a todo"),
            attribute.attribute("aria-label", "New todo"),
            attribute.attribute("autocomplete", "off"),
            attribute.attribute("enterkeyhint", "go"),
            attribute.attribute("required", ""),
          ]),
          html.button(
            [
              attribute.attribute("class", "btn btn-primary etodo-quick-add"),
              attribute.attribute("type", "submit"),
              attribute.attribute("aria-label", "Add todo"),
              attribute.title("Add todo"),
            ],
            [html.text("+")],
          ),
        ]),
        html.div(
          [
            attribute.attribute("id", "quick-add-status"),
            attribute.attribute("class", "small text-danger mt-1"),
            attribute.attribute("role", "status"),
          ],
          [],
        ),
      ],
    ),
    html.details([attribute.attribute("id", "new-todo")], [
      html.summary(
        [
          attribute.attribute(
            "class",
            "px-3 pb-2 small text-body-secondary etodo-new-summary",
          ),
        ],
        [html.text("More options")],
      ),
      html.div([attribute.attribute("class", "card-body pt-0 px-3 pb-3")], [
        html.form(
          [
            attribute.attribute("id", "new-todo-form"),
            attribute.attribute("action", "/todo/create"),
            attribute.attribute("method", "post"),
            attribute.attribute("class", "row g-3"),
          ],
          [
            hidden("csrf", data.csrf),
            hidden("list", data.selected_list),
            edit_textarea(scope, "Description", "description", ""),
            edit_textarea(scope, "Comment", "comment", ""),
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
        edit_textarea(scope, "Description", "description", task.description),
        edit_textarea(scope, "Comment", "comment", task.comment),
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
        list_picker(scope, data.lists, task.lists_db),
        reminder_editor(scope, task.reminder),
        html.div([attribute.attribute("class", "col-12")], [
          html.button(
            [
              attribute.attribute("class", "btn btn-primary"),
              attribute.attribute("type", "submit"),
            ],
            [html.text("Save changes")],
          ),
        ]),
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

fn reminder_text(reminder: Reminder) -> String {
  case reminder.enabled {
    False -> ""
    True -> {
      let when = string.trim(reminder.date <> " " <> reminder.time)
      let repeat = case list.key_find(recurrences, reminder.recurrence) {
        Ok("Once") | Error(_) -> ""
        Ok(text) -> ", " <> string.lowercase(text)
      }
      let until = case reminder.end_date {
        "" -> ""
        date -> " until " <> date
      }
      let next = case reminder.next {
        "" -> ""
        next -> " (next " <> next <> ")"
      }
      when <> repeat <> until <> next
    }
  }
}

fn reminder_editor(scope: String, reminder: Reminder) {
  let id = fn(name) { scope <> "-" <> name }
  let check = fn(name, label, checked) {
    html.div([attribute.attribute("class", "form-check")], [
      html.input(
        list.flatten([
          [
            attribute.attribute("class", "form-check-input"),
            attribute.attribute("type", "checkbox"),
            attribute.id(id(name)),
            attribute.name(name),
          ],
          case checked {
            True -> [attribute.attribute("checked", "")]
            False -> []
          },
        ]),
      ),
      html.label(
        [
          attribute.attribute("class", "form-check-label"),
          attribute.attribute("for", id(name)),
        ],
        [html.text(label)],
      ),
    ])
  }
  let repeat =
    recurrences
    |> list.map(fn(choice) {
      let #(value, label) = choice
      html.option(
        list.flatten([
          [attribute.attribute("value", value)],
          case value == reminder.recurrence {
            True -> [attribute.attribute("selected", "")]
            False -> []
          },
        ]),
        label,
      )
    })
  let summary = case reminder_text(reminder) {
    "" -> "Reminder: off"
    text -> "Reminder: " <> text
  }

  html.div([attribute.attribute("class", "col-12")], [
    html.details(
      list.flatten([
        [attribute.attribute("class", "etodo-reminder border rounded p-2")],
        case reminder.enabled {
          True -> [attribute.attribute("open", "")]
          False -> []
        },
      ]),
      [
        html.summary([], [html.text(summary)]),
        html.div([attribute.attribute("class", "row g-3 pt-2")], [
          html.div([attribute.attribute("class", "col-12")], [
            check("reminder", "Remind me", reminder.enabled),
          ]),
          edit_input(scope, "Date", "reminder_date", reminder.date, "date"),
          edit_input(scope, "Time", "reminder_time", reminder.time, "time"),
          html.div([attribute.attribute("class", "col-md-6")], [
            html.label(
              [
                attribute.attribute("class", "form-label"),
                attribute.attribute("for", id("reminder_recurrence")),
              ],
              [html.text("Repeat")],
            ),
            html.select(
              [
                attribute.attribute("class", "form-select"),
                attribute.id(id("reminder_recurrence")),
                attribute.name("reminder_recurrence"),
              ],
              repeat,
            ),
          ]),
          edit_input(
            scope,
            "Repeat until",
            "reminder_end_date",
            reminder.end_date,
            "date",
          ),
        ]),
      ],
    ),
  ])
}

fn list_picker(scope: String, available: List(String), selected: List(String)) {
  let checkbox = fn(name: String, index: Int) {
    let id = scope <> "-list-" <> int.to_string(index)
    let always = name == "All tasks"
    html.div([attribute.attribute("class", "form-check etodo-list-option")], [
      html.input(
        list.flatten([
          [
            attribute.attribute("class", "form-check-input"),
            attribute.attribute("type", "checkbox"),
            attribute.id(id),
            attribute.attribute("value", name),
          ],
          case always {
            True -> [attribute.attribute("disabled", "")]
            False -> [attribute.name("lists")]
          },
          case always || list.contains(selected, name) {
            True -> [attribute.attribute("checked", "")]
            False -> []
          },
        ]),
      ),
      case always {
        True ->
          html.input([
            attribute.attribute("type", "hidden"),
            attribute.name("lists"),
            attribute.attribute("value", name),
          ])
        False -> element.none()
      },
      html.label(
        [
          attribute.attribute("class", "form-check-label"),
          attribute.attribute("for", id),
        ],
        [html.text(name)],
      ),
    ])
  }
  let chosen =
    list.filter(available, fn(name) {
      name == "All tasks" || list.contains(selected, name)
    })
  html.div([attribute.attribute("class", "col-12 etodo-list-picker")], [
    html.div([attribute.attribute("class", "form-label")], [
      html.text("Lists"),
    ]),
    html.div([attribute.attribute("class", "etodo-list-summary small mb-1")], [
      html.text("In: " <> string.join(chosen, ", ")),
    ]),
    html.input([
      attribute.attribute(
        "class",
        "form-control form-control-sm mb-1 etodo-list-filter",
      ),
      attribute.attribute("type", "search"),
      attribute.attribute("placeholder", "Find a list"),
      attribute.attribute("autocomplete", "off"),
      attribute.attribute("aria-label", "Find a list"),
    ]),
    html.div(
      [attribute.attribute("class", "etodo-list-options border rounded p-2")],
      list.index_map(available, fn(name, index) { checkbox(name, index) }),
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
  let id = scope <> "-" <> name
  html.div(
    [
      attribute.attribute("class", "col-12 etodo-markdown-editor"),
      attribute.attribute("data-markdown-editor", ""),
    ],
    [
      html.div(
        [
          attribute.attribute(
            "class",
            "d-flex align-items-center justify-content-between gap-2 mb-2",
          ),
        ],
        [
          html.label(
            [
              attribute.attribute("class", "form-label mb-0"),
              attribute.attribute("for", id),
            ],
            [html.text(label)],
          ),
          html.div(
            [
              attribute.attribute("class", "btn-group btn-group-sm"),
              attribute.attribute("role", "group"),
              attribute.attribute("aria-label", label <> " view"),
            ],
            [
              markdown_mode_button("text", True),
              markdown_mode_button("preview", False),
            ],
          ),
        ],
      ),
      html.textarea(
        [
          attribute.attribute("class", "form-control"),
          attribute.attribute("id", id),
          attribute.name(name),
          attribute.attribute("rows", "3"),
          attribute.attribute("data-required", case scope {
            "new" -> "1"
            _ -> ""
          }),
          attribute.attribute("data-markdown-text", ""),
        ],
        value,
      ),
      html.div(
        [
          attribute.attribute("class", "form-control etodo-markdown-preview"),
          attribute.attribute("data-markdown-preview", ""),
          attribute.attribute("hidden", ""),
          attribute.attribute("aria-live", "polite"),
        ],
        [],
      ),
    ],
  )
}

fn markdown_mode_button(mode: String, selected: Bool) {
  html.button(
    list.flatten([
      [
        attribute.attribute("class", case selected {
          True -> "btn btn-secondary"
          False -> "btn btn-outline-secondary"
        }),
        attribute.attribute("type", "button"),
        attribute.attribute("data-markdown-mode", mode),
        attribute.attribute("aria-pressed", case selected {
          True -> "true"
          False -> "false"
        }),
      ],
    ]),
    [
      html.text(case mode {
        "preview" -> "Preview"
        _ -> "Text"
      }),
    ],
  )
}

fn edit_select(
  scope: String,
  label: String,
  name: String,
  value: String,
  options: List(String),
) {
  // Tasks without a status or priority come as "", which must select N/A
  // rather than let the browser fall back to the first option.
  let value = case string.trim(value) {
    "" -> "N/A"
    value -> value
  }
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

fn status_chip(status: String) {
  let shown = case string.trim(status) {
    "" -> "N/A"
    value -> value
  }
  let class = case shown {
    "Planning" -> "etodo-chip etodo-chip-status-planning"
    "In progress" -> "etodo-chip etodo-chip-status-progress"
    "Done" -> "etodo-chip etodo-chip-status-done"
    _ -> "etodo-chip etodo-chip-status-other"
  }
  html.span(
    [
      attribute.attribute("class", class),
      attribute.attribute("data-field", "status"),
    ],
    [html.text(shown)],
  )
}

fn priority_chip(priority: String) {
  let class = case priority {
    "High" -> "etodo-chip etodo-chip-priority-high"
    "Medium" -> "etodo-chip etodo-chip-priority-medium"
    "Low" -> "etodo-chip etodo-chip-priority-low"
    _ -> ""
  }
  case class {
    "" -> element.none()
    _ -> html.span([attribute.attribute("class", class)], [html.text(priority)])
  }
}

fn due_date_chip(due_date: String) {
  case string.trim(due_date) {
    "" -> element.none()
    date ->
      html.span(
        [
          attribute.attribute("class", "etodo-chip etodo-chip-due"),
          attribute.attribute("data-due-date", date),
        ],
        [html.text("Due " <> date)],
      )
  }
}

fn reminder_chip(reminder: Reminder) {
  case reminder_text(reminder) {
    "" -> element.none()
    text ->
      html.span(
        [attribute.attribute("class", "etodo-chip etodo-chip-reminder")],
        [html.text("Reminder · " <> text)],
      )
  }
}

fn progress_indicator(progress: String) {
  case int.parse(string.trim(progress)) {
    Ok(value) if value > 0 && value <= 100 ->
      html.div([attribute.attribute("class", "etodo-progress")], [
        html.progress(
          [
            attribute.attribute("class", "etodo-progress-track"),
            attribute.attribute("aria-label", "Progress"),
            attribute.attribute("value", int.to_string(value)),
            attribute.attribute("max", "100"),
          ],
          [html.text(int.to_string(value) <> "%")],
        ),
        html.span([attribute.attribute("class", "etodo-progress-label")], [
          html.text("Progress " <> int.to_string(value) <> "%"),
        ]),
      ])
    _ -> element.none()
  }
}

fn lists_metadata(lists: List(String)) {
  let visible_lists =
    lists
    |> list.filter(fn(name) { string.trim(name) != "" && name != "All tasks" })
    |> list.map(fn(name) {
      html.span([attribute.attribute("class", "etodo-list-tag")], [
        html.text(name),
      ])
    })
  case visible_lists {
    [] -> []
    _ -> [metadata_item_content("Lists", visible_lists)]
  }
}

fn time_metadata(estimate: String, remaining: String) {
  let has_estimate = has_nonzero_value(estimate)
  let has_remaining = has_nonzero_value(remaining)
  case has_estimate || has_remaining {
    False -> []
    True -> {
      let parts = case has_estimate, has_remaining {
        True, True -> estimate <> " h estimated · " <> remaining <> " h left"
        True, False -> estimate <> " h estimated"
        False, True -> remaining <> " h remaining"
        False, False -> ""
      }
      [metadata_item("Time", parts)]
    }
  }
}

fn owner_metadata(owner: String, shared_with: String) {
  case string.trim(shared_with) {
    "" -> []
    shared -> {
      let owner_item = case string.trim(owner) {
        "" -> []
        value -> [metadata_item("Owner", value)]
      }
      case shared == string.trim(owner) {
        True -> owner_item
        False -> list.append(owner_item, [metadata_item("Shared with", shared)])
      }
    }
  }
}

fn subtasks_metadata(has_subtasks: Bool) {
  case has_subtasks {
    True -> [metadata_item("Subtasks", "Has subtasks")]
    False -> []
  }
}

fn done_time_metadata(task: Todo) {
  let hidden = !task.done || string.trim(task.done_time) == ""
  let attrs = case hidden {
    True -> [
      attribute.attribute("class", "etodo-meta-item"),
      attribute.attribute("data-detail", "done-time"),
      attribute.attribute("hidden", ""),
    ]
    False -> [
      attribute.attribute("class", "etodo-meta-item"),
      attribute.attribute("data-detail", "done-time"),
    ]
  }
  [
    html.div(attrs, [
      html.span([attribute.attribute("class", "etodo-meta-label")], [
        html.text("Done"),
      ]),
      html.span(
        [
          attribute.attribute("class", "etodo-meta-value"),
          attribute.attribute("data-field", "done-time"),
        ],
        [html.text(task.done_time)],
      ),
    ]),
  ]
}

fn created_metadata(created: String) {
  let shown = case string.trim(created) {
    "" -> "date unavailable"
    value -> value
  }
  let children = case string.trim(created) {
    "" -> [html.text("Created " <> shown)]
    value -> [
      html.text("Created " <> value),
      html.span(
        [
          attribute.attribute("class", "etodo-created-age"),
          attribute.attribute("data-created-age", ""),
        ],
        [],
      ),
    ]
  }
  let attrs = case string.trim(created) {
    "" -> [attribute.attribute("class", "etodo-created")]
    value -> [
      attribute.attribute("class", "etodo-created"),
      attribute.attribute("data-created-date", value),
    ]
  }
  html.div(attrs, children)
}

fn has_nonzero_value(value: String) {
  case string.trim(value) {
    "" | "0" | "0.0" | "0.00" | "0.000" -> False
    _ -> True
  }
}

fn metadata_item(label: String, value: String) {
  metadata_item_content(label, [html.text(value)])
}

fn metadata_item_content(label: String, content: List(element.Element(a))) {
  html.div([attribute.attribute("class", "etodo-meta-item")], [
    html.span([attribute.attribute("class", "etodo-meta-label")], [
      html.text(label),
    ]),
    html.div([attribute.attribute("class", "etodo-meta-value")], content),
  ])
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
  use can_reorder <- decode.field("can_reorder", decode.bool)
  use todos <- decode.field("todos", decode.list(of: todo_decoder()))
  use filtered_out <- decode.field("filtered_out", decode.int)
  use show_all <- decode.field("show_all", decode.bool)
  decode.success(TodoPageData(
    lists:,
    selected_list:,
    csrf:,
    can_edit:,
    can_reorder:,
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
  use lists_db <- decode.field("lists_db", decode.list(decode.string))
  use estimate <- decode.field("estimate", decode.string)
  use remaining <- decode.field("remaining", decode.string)
  use reminder <- decode.optional_field(
    "reminder",
    no_reminder,
    reminder_decoder(),
  )
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
    lists_db:,
    estimate:,
    remaining:,
    reminder:,
  ))
}

fn reminder_decoder() -> decode.Decoder(Reminder) {
  use enabled <- decode.field("enabled", decode.bool)
  use date <- decode.field("date", decode.string)
  use time <- decode.field("time", decode.string)
  use recurrence <- decode.field("recurrence", decode.string)
  use end_date <- decode.field("end_date", decode.string)
  use next <- decode.field("next", decode.string)
  decode.success(Reminder(enabled:, date:, time:, recurrence:, end_date:, next:))
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
