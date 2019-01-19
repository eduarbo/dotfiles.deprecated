unmapAllExcept(['/', '.', 'cs', 'cS', 'n', 'N', 'd', 'e', 'gg', 'G', 'j', 'k', 'v']);

//
// Helpers

const GROUP = {
  HELP: 0,
  MOUSE_CLICK: 1,
  SCROLL_PAGE: 2,
  TABS: 3,
  PAGE: 4,
  SESSIONS: 5,
  SEARCH: 6,
  CLIPBOARD: 7,
  OMNIBAR: 8,
  VISUAL: 9,
  VIM: 10,
  SETTINGS: 11,
  CHROME: 12,
  PROXY: 13,
  MISC: 14,
  INSERT: 15
};

function keymap(group, fn) {
  const bind = mapkeyFn => (keys, annotation, cb, options) => {
    [].concat(keys).forEach((key) => {
      mapkeyFn(key, `#${group}${annotation}`, cb, options);
    })
  }

  const helpers = {
    normal: bind(mapkey),
    visual: bind(vmapkey),
    insert: bind(imapkey),
  };

  return fn(helpers)
}

//
// Bindings

keymap(GROUP.HELP, ({ normal }) => {
  // That way I have all site keyboard shortcuts available under the leader key
  // e.g. to use GitHub original shortcut to go to Pull Requests page just type `,gp`
  normal(',', 'Temporarily suppress SurfingKeys', Normal.passThrough);
  normal('?', 'Show usage', Front.showUsage);
});

keymap(GROUP.MOUSE_CLICK, ({ visual, normal }) => {
  const openLink = (opts, selector = '') => () => Hints.create(selector, Hints.dispatchMouseClick, opts);

  normal('f',  'Open a link in current tab', openLink());
  normal('F',  'Open a link in new tab', openLink({ tabbed: true, active: false }));

  normal('gi', 'Go to the first edit box', Hints.createInputLayer);
  normal('i', 'Go to edit box', () => {
    Hints.create("input, textarea, *[contenteditable=true], select", Hints.dispatchMouseClick);
  });
  normal('I', 'Go to edit box with vim editor', () => {
    Hints.create('input, textarea, *[contenteditable=true], select', element => Front.showEditor(element));
  });

  // FIXME
  normal('[[', 'Click on the previous link on current page', previousPage);
  normal(']]', 'Click on the next link on current page', nextPage);

  normal('oF', 'Open multiple links in a new tab', openLink({ multipleHits: true }));
  normal('of', 'Click on an Image or a button', () => Hints.create('img, button', Hints.dispatchMouseClick));
  normal('gm', 'mouse out last element', () => Hints.mouseoutLastElement());
  normal('gh', 'Mouse over elements.', () => {
    Hints.create('', Hints.dispatchMouseClick, { mouseEvents: ['mouseover'] });
  });
  normal('gH', 'Mouse out elements.', () => {
    Hints.create('', Hints.dispatchMouseClick, { mouseEvents: ['mouseout'] });
  });
  normal('gl', 'Open detected links from text', () => {
    Hints.create(runtime.conf.clickablePat, (element) => {
      createElement(`<a href=${element[2]}>`).click();
    }, { statusLine: 'Open detected links from text' });
  });
});

keymap(GROUP.SCROLL_PAGE, ({ normal }) => {
  normal('gf', 'Switch frames', Normal.rotateFrame);
  normal('gF', 'Focus top window', top.focus);

  // Scroll page up
  map('K', 'e');
  // Scroll page down
  map('J', 'd');
  // FIXME: Ctrl bindings not working
  // map('<Ctrl-u>', 'e');
  // map('<Ctrl-d>', 'd');
  unmap('e');
  unmap('d');

  // Change scroll target
  map('gs', 'cs');
  // Reset scroll target
  map('gS', 'cS');
  // Unmap default mappings
  unmap('cs');
  unmap('cS');
});

keymap(GROUP.TABS, ({ normal }) => {
  const goHistoryTab = (opts) => () => RUNTIME('historyTab', opts);
  normal('h', 'Go to previous tab', () => RUNTIME("previousTab"));
  normal('l', 'Go to next tab', () => RUNTIME("nextTab"));
  normal('{', 'Go one tab history back', goHistoryTab({ backward: true }), { repeatIgnore: true });
  normal('}', 'Go one tab history forward', goHistoryTab({ backward: true }), { repeatIgnore: true });
  normal('`', 'Go to last used tab', () => RUNTIME('goToLastTab'));
  normal('xx', 'Close current tab', () => RUNTIME("closeTab"));
  normal('X', 'Restore closed tab', () => RUNTIME('openLast'));
  normal('<<', 'Move current tab to left', () => RUNTIME('moveTab', { step: -1 }));
  normal('>>', 'Move current tab to right', () => RUNTIME('moveTab', { step: 1 }));
  normal('W', 'New window with current tab',  () => RUNTIME('newWindow'));
  normal('<Space>', 'Choose a tab', Front.chooseTab);
  normal('zr', 'zoom reset', () => RUNTIME('setZoom', { zoomFactor: 0 }));
  normal('zi', 'zoom in', () => RUNTIME('setZoom', { zoomFactor: 0.1 }));
  normal('zo', 'zoom out', () => RUNTIME('setZoom', { zoomFactor: -0.1 }));
});

keymap(GROUP.PAGE, ({ normal }) => {
  normal('r', 'Reload the page', () => RUNTIME('reloadTab', { nocache: false }));
  normal('R', 'Reload the page without cache', () => RUNTIME('reloadTab', { nocache: true }));
  normal('H', 'Go back in history', () => history.go(-1), {repeatIgnore: true});
  normal('L', 'Go forward in history', () => history.go(1), {repeatIgnore: true});
  normal('su', 'Edit current URL with vim and reload', () => {
    Front.showEditor(window.location.href, (data) => window.location.href = data, 'url');
  });
  normal('sU', 'Edit current URL with vim and open in new tab', () => {
    Front.showEditor(window.location.href, (data) => tabOpenLink(data), 'url');
  });
});

keymap(GROUP.MISC, ({ normal }) => {
  normal('gB', 'Remove bookmark for current page', () => RUNTIME('removeBookmark'));
  normal('gb', 'Bookmark current page to selected folder', () => {
    const extra = { url: window.location.href, title: document.title };
    Front.openOmnibar(({ type: "AddBookmark", extra }));
  });

  injectKillElementHintStyle();

  normal('d', 'Kill element', killElementWithStyle);
  normal('D', 'Kill multiple element', () => killElementWithStyle({ multipleHits: true }));

  // FIXME: Don't press ESC twice to exit
  function killElementWithStyle(hintOptions = {}) {
    const className = 'sk_trial';
    const $body = document.querySelector('body');

    Hints.create('*', (element) => {
      element.parentNode.removeChild(element);
      if (!hintOptions.multipleHits) {
        handleHintsExit();
      }
    }, hintOptions);

    $body.classList.add(className);

    document.addEventListener('keydown', handleEsc);

    function handleHintsExit() {
      $body.classList.remove(className)
      document.removeEventListener('keydown', handleEsc);
    }

    function handleEsc(event) {
      if (event.key === 'Escape') {
        handleHintsExit();
      }
    };
  }

  function injectKillElementHintStyle() {
    const $css = document.createElement('style');
    $css.type = 'text/css';

    const styles = '.sk_trial * { outline: 1px dashed red; }';
    $css.appendChild(document.createTextNode(styles));
    const $head = document.querySelector('head');
    $head.appendChild($css);
  }
});

keymap(GROUP.INSERT, ({ normal }) => {
  normal('<Ctrl-i>', 'Open vim editor for current input', () => {
    const element = getRealEdit();
    element.blur();
    Insert.exit();
    Front.showEditor(element);
  });
});

keymap(GROUP.VISUAL, ({ visual, normal }) => {
  normal('V', 'Restore visual mode', Visual.restore);
  normal('gv', 'Enter visual mode, and select whole element', () => Visual.toggle('z'));
  visual('*', 'Find selected text in current page', () => {
    Visual.star();
    Visual.toggle();
  });

  // FIXME: Ctrl bindings not working
  visual('<Ctrl-u>', 'Backward 20 lines', () => Visual.feedkeys('20k'));
  visual('<Ctrl-d>', 'Forward 20 lines', () => Visual.feedkeys('20j'));
});

keymap(GROUP.CLIPBOARD, ({ normal }) => {
  normal('cd', "Copy current downloading URL", () => {
    const opts = { action: 'getDownloads', query: { state: "in_progress" } };
    runtime.command(opts, ({ downloads }) => {
      Clipboard.write(downloads.map(o => o.url).join(','));
    });
  });
  normal('cc', "Copy current page's URL", () => Clipboard.write(window.location.href));
  normal('ch', "Copy current page's host", () => {
    const url = new URL(window.location.href);
    Clipboard.write(url.host);
  });
  normal('cv', 'Copy text of an element', () => Visual.toggle("y"));
  normal('cV', 'Copy text of multiple elements', () => Visual.toggle("ym"));
  normal('ci', 'Copy text of an input', () => {
    Hints.create("input, textarea, select", ({ value }) => Clipboard.write(value));
  });
  normal('cq', 'Copy pre text', () => {
    Hints.create("pre", ({ innerText }) => Clipboard.write(innerText));
  });
  normal('cl', 'Copy a link URL to the clipboard', () => {
    Hints.create('*[href]', element => Clipboard.write(element.href));
  });
  normal('cL', 'Copy multiple link URLs to the clipboard', () => {
    const linksToYank = [];
    Hints.create('*[href]', ({ href }) => {
      linksToYank.push(href);
      Clipboard.write(linksToYank.join('\n'));
    }, { multipleHits: true });
  });

  normal(['cs', 'gc'], 'Open selected link or link from clipboard', () => {
    const selection = window.getSelection().toString();
    if (selection) {
      tabOpenLink(selection);
    } else {
      Clipboard.read(({ data }) => tabOpenLink(data));
    }
  });
});

keymap(GROUP.VIM, ({ normal }) => {
  normal('m', 'Add current URL to vim-like marks', Normal.addVIMark);
  normal("'", 'Jump to vim-like mark', Normal.jumpVIMark);
  normal("g'", 'Jump to vim-like mark in new tab', mark => Normal.jumpVIMark(mark, true));
})

keymap(GROUP.OMNIBAR, ({ normal, visual }) => {
  normal(';', 'Open commands', () => {
    Front.openOmnibar({ type: "Commands" });
  });
  normal('<Enter>', 'Open a URL', () => {
    Front.openOmnibar({ type: "URLs", extra: "getAllSites", tabbed: false });
  });
  normal('<Shift-Enter>', 'Open a URL in new tab', () => {
    Front.openOmnibar({ type: "URLs", extra: "getAllSites" });
  });
  normal('ox', 'Open recently closed URL', () => {
    Front.openOmnibar({ type: "URLs", extra: "getRecentlyClosed" });
  });
  normal(['ou', 'u'], 'Open opened URL', () => {
    Front.openOmnibar({ type: "URLs", extra: "getTabURLs", tabbed: false });
  });
  normal(['oU', 'U'], 'Open opened URL in new tab', () => {
    Front.openOmnibar({ type: "URLs", extra: "getTabURLs" });
  });
  normal(['ob', 'b'], 'Open a bookmark', () => {
    Front.openOmnibar({ type: "Bookmarks", tabbed: false });
  });
  normal(['oB', 'B'], 'Open a bookmark in new tab', () => {
    Front.openOmnibar({ type: "Bookmarks" });
  });
  normal('oy', 'Open URL from history', () => {
    Front.openOmnibar({ type: "History", tabbed: false });
  });
  normal('oY', 'Open URL from history in new tab', () => {
    Front.openOmnibar({ type: "History" });
  });
  normal('om', 'Open URL from vim-like marks', () => {
    Front.openOmnibar({ type: "VIMarks" });
  });
  normal('oi', 'Open incognito window', () => {
    runtime.command({ action: 'openIncognito', url: window.location.href });
  });

  const mapSearchAlias = (alias, prompt, url, options = {}, cb) => {
    const { suggestionUrl, prefix = 'o', searchLeaderKey = 's', mapToSearchSelected = true } = options
    const aliasNewTab = alias.toUpperCase();
    const searchEngine = (extra, tabbed) => () => {
      Front.openOmnibar({ type: 'SearchEngine', extra, tabbed });
    };

    normal(`${prefix}${alias}`, `Search with ${prompt}`, searchEngine(alias, false));
    normal(`${prefix}${aliasNewTab}`, `Search with ${prompt} in new tab`, searchEngine(alias));
    addSearchAlias(alias, prompt, url, suggestionUrl, cb, prefix);

    if (mapToSearchSelected) {
      keymap(GROUP.SEARCH, ({ normal, visual }) => {
        const searchMapping = `${searchLeaderKey}${alias}`;
        const searchSelectedWithUrl = () => searchSelectedWith(url);
        normal(searchMapping, `Search from clipboard with ${prompt}`, searchSelectedWithUrl);
        visual(searchMapping, `Search Selected with ${prompt}`, searchSelectedWithUrl);
      });
    }
  };

  mapSearchAlias('g', 'Google', 'https://www.google.com/search?q=', {
    suggestionUrl: 'https://www.google.com/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q='
  }, (response) => {
    const res = JSON.parse(response.text);
    return res[1];
  });

  mapSearchAlias('d', 'DuckDuckGo', 'https://duckduckgo.com/?q=', {
    suggestionUrl: 'https://duckduckgo.com/ac/?q='
  }, (response) => {
    const res = JSON.parse(response.text);
    return res.map(r => r.phrase);
  });

  mapSearchAlias('v', 'YouTube', 'https://www.youtube.com/results?search_query=', {
    suggestionUrl: 'https://clients1.google.com/complete/search?client=youtube&ds=yt&callback=cb&q='
  }, (response) => {
    const res = JSON.parse(response.text.substr(9, response.text.length - 10));
    return res[1].map(d => d[0]);
  });

  mapSearchAlias('s', 'StackOverflow', 'http://stackoverflow.com/search?q=');
  mapSearchAlias('h', 'GitHub', 'https://github.com/search?type=Code&utf8=%E2%9C%93&q=');

  // TODO: Implement subreddit suggestions
  mapSearchAlias('r', 'subReddit', 'https://reddit.com/r/', { mapToSearchSelected: false });
});

keymap(GROUP.CHROME, ({ normal }) => {
  normal('oca', 'Open Chrome About', () => tabOpenLink("chrome://help/"));
  normal('ocb', 'Open Chrome Bookmarks', () => tabOpenLink("chrome://bookmarks/"));
  normal('occ', 'Open Chrome Cache', () => tabOpenLink("chrome://cache/"));
  normal('ocd', 'Open Chrome Downloads', () => tabOpenLink("chrome://downloads/"));
  normal('ocy', 'Open Chrome History', () => tabOpenLink("chrome://history/"));
  normal('ock', 'Open Chrome Cookies', () => tabOpenLink("chrome://settings/content/cookies"));
  normal('oce', 'Open Chrome Extensions', () => tabOpenLink("chrome://extensions/"));
  normal('ocn', 'Open Chrome net-internals', () => tabOpenLink("chrome://net-internals/#proxy"));
  normal('oci', 'Open Chrome Inspect', () => tabOpenLink("chrome://inspect/#devices"));
});


//
// Settings

mySettings = {
  smoothScroll: false,
  hintAlign: 'left'
};

Object.assign(settings, mySettings);

// Theme

settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #24272e;
    color: #abb2bf;
}
.sk_theme tbody {
    color: #fff;
}
.sk_theme input {
    color: #d0d0d0;
}
.sk_theme .url {
    color: #61afef;
}
.sk_theme .annotation {
    color: #56b6c2;
}
.sk_theme .omnibar_highlight {
    color: #528bff;
}
.sk_theme .omnibar_timestamp {
    color: #e5c07b;
}
.sk_theme .omnibar_visitcount {
    color: #98c379;
}
.sk_theme #sk_omnibarSearchResult>ul>li:nth-child(odd) {
    background: #303030;
}
.sk_theme #sk_omnibarSearchResult>ul>li.focused {
    background: #3e4452;
}
#sk_status, #sk_find {
    font-size: 20pt;
}`;
