(function () {
  "use strict";

  function updateCount() {
    var count = document.getElementById("todo-count");
    if (count) {
      var visible = document.querySelectorAll(
        "#todo-items > li[data-uid]:not([hidden])"
      ).length;
      count.textContent = visible + " shown";
    }
  }

  var filterBox = document.getElementById("todo-filter");
  if (filterBox) {
    var applyFilter = function () {
      var words = filterBox.value.toLowerCase().split(/\s+/).filter(Boolean);
      document
        .querySelectorAll("#todo-items > li[data-uid]")
        .forEach(function (item) {
          var text = (item.getAttribute("data-search") || "").toLowerCase();
          item.hidden = !words.every(function (word) {
            return text.indexOf(word) !== -1;
          });
        });
      updateCount();
    };
    filterBox.addEventListener("input", applyFilter);
    applyFilter();
  }

  document.addEventListener("submit", function (event) {
    var form = event.target;
    if (form.classList && form.classList.contains("etodo-delete-form")) {
      if (!window.confirm("Delete this task permanently?")) {
        event.preventDefault();
        return;
      }
      event.preventDefault();
      var button = document.querySelector(
        'button[form="' + form.id + '"][type=submit]'
      );
      var card = form.closest("li");
      button.disabled = true;
      fetch(form.action, {
        method: "POST",
        credentials: "same-origin",
        headers: {
          "Content-Type": "application/x-www-form-urlencoded",
          "X-Etodo-Fetch": "1"
        },
        body: new URLSearchParams(new FormData(form)).toString()
      })
        .then(function (response) {
          if (!response.ok) {
            throw new Error("HTTP " + response.status);
          }
          card.remove();
          var tasks = document.querySelectorAll("#todo-items > li[data-uid]");
          updateCount();
          if (tasks.length === 0) {
            var empty = document.createElement("li");
            empty.className =
              "card border-0 shadow-sm text-center etodo-empty-state";
            var body = document.createElement("div");
            body.className = "card-body p-4 p-md-5";
            var mark = document.createElement("div");
            mark.className = "etodo-empty-mark mb-3";
            mark.textContent = "\u2713";
            var heading = document.createElement("h3");
            heading.className = "h5 fw-semibold";
            heading.textContent = "All clear";
            var message = document.createElement("p");
            message.className = "text-body-secondary mb-0";
            message.textContent = "There are no todos in this list yet.";
            body.append(mark, heading, message);
            empty.append(body);
            document.getElementById("todo-items").append(empty);
          }
        })
        .catch(function () {
          button.disabled = false;
          window.alert("Could not delete the task. Please try again.");
        });
      return;
    }
    if (!form.classList || !form.classList.contains("etodo-done-form")) {
      return;
    }
    event.preventDefault();

    var done = form.elements["done"];
    var wantDone = done.value === "true";
    var button = form.querySelector(".etodo-check");
    var title = form.parentNode.querySelector(".etodo-title");
    button.disabled = true;

    fetch(form.action, {
      method: "POST",
      credentials: "same-origin",
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
        "X-Etodo-Fetch": "1"
      },
      body: new URLSearchParams(new FormData(form)).toString()
    })
      .then(function (response) {
        if (!response.ok) {
          throw new Error("HTTP " + response.status);
        }
        return response.text();
      })
      .then(function (doneTime) {
        var card = form.closest("li");
        var setField = function (name, text) {
          var cell = card.querySelector('[data-field="' + name + '"]');
          if (cell) {
            cell.textContent = text || "-";
          }
        };
        setField("status", wantDone ? "Done" : "Planning");
        setField("done-time", wantDone ? doneTime : "");
        button.classList.toggle("etodo-check-on", wantDone);
        button.setAttribute("aria-checked", wantDone ? "true" : "false");
        var label = wantDone ? "Mark as not done" : "Mark as done";
        button.setAttribute("aria-label", label);
        button.title = label;
        button.textContent = wantDone ? "\u2713" : "";
        title.classList.toggle("etodo-done", wantDone);
        done.value = wantDone ? "false" : "true";
        button.disabled = false;
      })
      .catch(function () {
        // Fall back to a normal form post, which reloads the page.
        form.submit();
      });
  });
})();

(function () {
  var page = document.getElementById("status-page");
  if (!page) {
    return;
  }
  var filled = false;
  var set = function (id, text) {
    document.getElementById(id).textContent = text;
  };
  var pad = function (n) {
    return (n < 10 ? "0" : "") + n;
  };
  var show = function (data) {
    document.body.className =
      "etodo-status etodo-status-" +
      String(data.status).toLowerCase().replace(/[^a-z]+/g, "-");
    set("status-user", data.user);
    var select = document.getElementById("status-select");
    if (select && !filled) {
      filled = true;
      select.value = data.status;
      document.getElementById("status-message").value = data.status_msg;
    }
    set("status-state", data.status);
    set("status-msg", data.status_msg);
    set("status-timer-desc", data.timer_desc);
    var s = data.timer;
    set(
      "status-timer",
      s > 0
        ? pad(Math.floor(s / 3600)) + ":" + pad(Math.floor(s / 60) % 60) + ":" + pad(s % 60)
        : "",
    );
  };
  var poll = function () {
    fetch("/status.json", { credentials: "same-origin", cache: "no-store" })
      .then(function (response) {
        if (response.status === 401) {
          window.location.href = "/";
        }
        if (!response.ok) {
          throw new Error("HTTP " + response.status);
        }
        return response.json();
      })
      .then(show)
      .catch(function () {
        set("status-state", "Not connected");
      });
  };
  poll();
  setInterval(poll, 1000);
})();
