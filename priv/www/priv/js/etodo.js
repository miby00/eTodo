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

  var listSelect = document.getElementById("todo-list");
  if (listSelect && listSelect.form) {
    listSelect.addEventListener("change", function () {
      listSelect.form.submit();
    });
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

(function () {
  var pickerOf = function (element) {
    return element.closest ? element.closest(".etodo-list-picker") : null;
  };
  var updateSummary = function (picker) {
    var names = [];
    picker
      .querySelectorAll(".etodo-list-option input[type=checkbox]")
      .forEach(function (box) {
        if (box.checked) {
          names.push(box.value);
        }
      });
    picker.querySelector(".etodo-list-summary").textContent =
      "In: " + names.join(", ");
  };
  document.addEventListener("input", function (event) {
    if (!event.target.classList.contains("etodo-list-filter")) {
      return;
    }
    var picker = pickerOf(event.target);
    var words = event.target.value.toLowerCase().split(/\s+/).filter(Boolean);
    picker.querySelectorAll(".etodo-list-option").forEach(function (option) {
      var name = option.textContent.toLowerCase();
      var show = words.every(function (word) {
        return name.indexOf(word) !== -1;
      });
      option.classList.toggle("d-none", !show);
    });
  });
  document.addEventListener("change", function (event) {
    var picker = pickerOf(event.target);
    if (picker && event.target.type === "checkbox") {
      updateSummary(picker);
    }
  });
  document.addEventListener("keydown", function (event) {
    // Enter in the filter box must not submit the form.
    if (event.key === "Enter" && event.target.classList.contains("etodo-list-filter")) {
      event.preventDefault();
    }
  });
})();

(function () {
  "use strict";
  var panels = document.querySelector("main.panels");
  if (!panels) {
    return;
  }
  var tabs = document.querySelectorAll(".kinds a[href^='#']");
  var current = function () {
    var index = Math.round(panels.scrollLeft / Math.max(panels.clientWidth, 1));
    return panels.children[Math.min(index, panels.children.length - 1)];
  };
  var markTab = function () {
    var panel = current();
    if (panel && window.location.hash !== "#" + panel.id) {
      history.replaceState(null, "", "#" + panel.id);
    }
    tabs.forEach(function (tab) {
      if (panel && tab.getAttribute("href") === "#" + panel.id) {
        tab.setAttribute("aria-current", "page");
      } else {
        tab.removeAttribute("aria-current");
      }
    });
  };
  var initial = document.getElementById(
    window.location.hash.slice(1) || panels.getAttribute("data-selected")
  );
  if (initial && initial.parentElement === panels) {
    panels.scrollLeft = initial.offsetLeft;
  }
  markTab();
  panels.addEventListener("scroll", function () {
    window.requestAnimationFrame(markTab);
  }, { passive: true });
  tabs.forEach(function (tab) {
    tab.addEventListener("click", function (event) {
      var panel = document.getElementById(tab.getAttribute("href").slice(1));
      if (panel) {
        event.preventDefault();
        panels.scrollTo({ left: panel.offsetLeft, behavior: "smooth" });
        history.replaceState(null, "", "#" + panel.id);
      }
    });
  });
})();

(function () {
  "use strict";
  // Page-level swipe: body data-swipe-next/data-swipe-prev hold the targets.
  var body = document.body;
  if (!body || !("ontouchstart" in window)) {
    return;
  }
  var start = null;
  var scrollState = function (element) {
    var found = [];
    for (var node = element; node && node !== body; node = node.parentElement) {
      var overflow = window.getComputedStyle(node).overflowX;
      if ((overflow === "auto" || overflow === "scroll") &&
          node.scrollWidth > node.clientWidth + 1) {
        found.push({
          back: node.scrollLeft > 1,
          forward: node.scrollLeft + node.clientWidth < node.scrollWidth - 1
        });
      }
    }
    return found;
  };
  document.addEventListener("touchstart", function (event) {
    var target = event.target;
    if (event.touches.length !== 1 ||
        target.closest("input, textarea, select, [contenteditable]")) {
      start = null;
      return;
    }
    var touch = event.touches[0];
    start = {
      x: touch.clientX,
      y: touch.clientY,
      time: Date.now(),
      scrollers: scrollState(target)
    };
  }, { passive: true });
  document.addEventListener("touchend", function (event) {
    var swipe = start;
    start = null;
    if (!swipe) {
      return;
    }
    var touch = event.changedTouches[0];
    var dx = touch.clientX - swipe.x;
    var dy = touch.clientY - swipe.y;
    if (Math.abs(dx) < 80 || Math.abs(dx) < 2 * Math.abs(dy) ||
        Date.now() - swipe.time > 800) {
      return;
    }
    var forward = dx < 0;
    // Let horizontally scrollable content (panels, tables) take the swipe first.
    var inner = swipe.scrollers.some(function (state) {
      return forward ? state.forward : state.back;
    });
    var target = body.getAttribute(forward ? "data-swipe-next" : "data-swipe-prev");
    if (!inner && target) {
      window.location.href = target;
    }
  }, { passive: true });
})();

(function () {
  "use strict";
  // Viewing the messages page marks everything up to "seen" as read.
  var seenKey = "etodo-messages-seen";
  var onMessages = !!document.querySelector("main.panels");
  var button = document.querySelector("a.etodo-header-button[href^='/messages']");
  if (!onMessages && !button) {
    return;
  }
  var baseTitle = document.title;
  var loaded = null;
  var draft = document.querySelector(".composer textarea");
  var typing = function () {
    return !!draft && (draft.value.trim() !== "" || document.activeElement === draft);
  };
  if (draft && !("ontouchstart" in window)) {
    draft.addEventListener("keydown", function (event) {
      if (event.key === "Enter" && !event.shiftKey && !event.isComposing) {
        event.preventDefault();
        if (draft.value.trim() !== "") {
          draft.form.requestSubmit();
        }
      }
    });
  }
  var latestOf = function (data) {
    return Math.max(data.chat.latest, data.reminders.latest, data.system.latest);
  };
  var showBadge = function (count) {
    var badge = button.querySelector(".etodo-badge");
    if (count > 0) {
      if (!badge) {
        badge = document.createElement("span");
        badge.className = "etodo-badge";
        button.appendChild(badge);
      }
      badge.textContent = count > 99 ? "99+" : String(count);
      button.setAttribute("aria-label", "Messages, " + count + " new");
      document.title = "(" + count + ") " + baseTitle;
    } else {
      if (badge) {
        badge.remove();
      }
      button.setAttribute("aria-label", "Messages");
      document.title = baseTitle;
    }
  };
  var poll = function () {
    var stored = window.localStorage.getItem(seenKey);
    var since = stored === null ? 0 : Number(stored);
    fetch("/messages.json?since=" + since, {
      credentials: "same-origin",
      cache: "no-store"
    })
      .then(function (response) {
        if (!response.ok) {
          throw new Error("HTTP " + response.status);
        }
        return response.json();
      })
      .then(function (data) {
        var latest = latestOf(data);
        if (onMessages) {
          window.localStorage.setItem(seenKey, String(latest));
          if (loaded === null) {
            loaded = latest;
          } else if (latest !== loaded && !typing()) {
            window.location.reload();
          }
        } else if (stored === null) {
          // First visit: start counting from now instead of flagging history.
          window.localStorage.setItem(seenKey, String(latest));
        } else {
          showBadge(data.chat["new"] + data.reminders["new"] + data.system["new"]);
        }
      })
      .catch(function () {});
  };
  poll();
  // The server pushes "messages" events; polling is only a slow safety net.
  if (window.EventSource) {
    var events = new EventSource("/events");
    events.addEventListener("messages", poll);
    events.addEventListener("open", poll);
  }
  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "visible") {
      poll();
    }
  });
  window.setInterval(poll, 60000);
})();

(function () {
  "use strict";
  // Work log, Time log and Schedule reload when logged time or alarms change.
  if (document.body.getAttribute("data-live") !== "reports" || !window.EventSource) {
    return;
  }
  var pending = null;
  var busy = function () {
    var active = document.activeElement;
    return !!active && /^(INPUT|SELECT|TEXTAREA)$/.test(active.tagName);
  };
  var reload = function () {
    pending = null;
    if (busy() || document.visibilityState !== "visible") {
      pending = true;
      return;
    }
    window.location.reload();
  };
  var events = new EventSource("/events");
  events.addEventListener("reports", function () {
    window.clearTimeout(pending);
    pending = window.setTimeout(reload, 300);
  });
  document.addEventListener("visibilitychange", function () {
    if (pending === true && document.visibilityState === "visible") {
      reload();
    }
  });
  document.addEventListener("focusout", function () {
    if (pending === true) {
      window.setTimeout(reload, 0);
    }
  });
})();

// Some mobile browsers report a viewport height that includes the system
// navigation bar, so the page is sized from the visible viewport instead.
(function () {
  "use strict";

  var root = document.documentElement;

  function setHeight() {
    var viewport = window.visualViewport;
    var height = viewport ? viewport.height : window.innerHeight;
    root.style.setProperty("--app-height", Math.floor(height) + "px");
  }

  setHeight();
  window.addEventListener("resize", setHeight);
  window.addEventListener("orientationchange", setHeight);
  if (window.visualViewport) {
    window.visualViewport.addEventListener("resize", setHeight);
  }
})();

// The status button in the header shows the current status as a colored dot.
(function () {
  "use strict";

  var icon = document.getElementById("status-icon");
  if (!icon) {
    return;
  }

  var dots = { available: "🟢", busy: "🔴", away: "🟠" };

  function update() {
    fetch("/status.json", { credentials: "same-origin", cache: "no-store" })
      .then(function (response) {
        return response.ok ? response.json() : null;
      })
      .then(function (data) {
        if (data) {
          icon.textContent = dots[String(data.status).toLowerCase()] || "⚪";
        }
      })
      .catch(function () {});
  }

  update();
  window.setInterval(update, 15000);
  document.addEventListener("visibilitychange", function () {
    if (!document.hidden) {
      update();
    }
  });
})();
