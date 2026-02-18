function on_page_load(header_class, icons) {
    set_reactable_search_placeholder("search...");
    insert_dropdown_icons(icons);
    hide_loading_screen(header_class);
};

function insert_dropdown_icons(icons) {
    let dropdown_class = ".primary-tabset";
    for (var i = 1; i <= icons.length; i++) {
        let icon = make_dropdown_icons(icons[i - 1]);
        let dropdown_entry = $(dropdown_class).parentElement.children[0].children[i - 1].children[0];
        dropdown_entry.prepend(icon);
    }
}

function make_dropdown_icons(icon_path) {
    var icon = document.createElement("img");
    icon.setAttribute("src", icon_path);
    icon.style.width = "40px";
    icon.style.marginRight = "5px";
    return icon;
}

function set_reactable_search_placeholder(text = 'search...') {
    $('.rt-filter').placeholder = text;
}

function hide_loading_screen(header_class) {
    let dropdown_class = ".primary-tabset";
    $('#loading').hidden = true;
    $(header_class).hidden = false;
    $(dropdown_class).hidden = false;
    $('#footer').hidden = false;
}