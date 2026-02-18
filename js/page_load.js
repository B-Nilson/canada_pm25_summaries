function on_page_load(header_class, icons) {
    set_reactable_search_placeholder("search...");
    insert_primary_tab_icons(icons);
    //hide_loading_screen(header_class); // TODO: remove if not needed for other reports
};

function insert_primary_tab_icons(icons) {
    let p_tabs_class = ".primary-tabset";
    for (var i = 0; i < icons.length; i++) {
        let icon = make_primary_tab_icons(icons[i]);
        let tab_i = $(p_tabs_class).children().first().children()[i].childNodes[0];
        tab_i.prepend(icon);
    }
}

function make_primary_tab_icons(icon_path) {
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