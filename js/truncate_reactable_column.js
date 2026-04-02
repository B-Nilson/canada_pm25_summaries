function add_truncate_listeners(keys) {
    let all_reactables = document.getElementsByClassName("reactable");
    Array.from(all_reactables).forEach(
        elem => Reactable.onStateChange(elem.id, state => truncate_reactable_columns(elem.id, keys))
    );
}

function truncate_reactable_columns(table_id, keys) {
    insert_aqmap_links();
    let table_data = Reactable.getState(table_id).data;
    table_data.map((row) => {
        keys.forEach((key) => {
            if(row[key]) row[key] = truncate_long_text(row[key]);
        });
        return row;
    });
    Reactable.setData(table_id, table_data);
};

function truncate_long_text(x, hover = x) {
    hover = hover.replace(/'/g, "&rsquo;").replace(/"/g, "&quot;");
    let styles = ["display:table; table-layout:fixed; width:100%", "overflow-x:hidden; text-overflow:ellipsis; white-space:nowrap"];
    return `<div style="${styles[0]}"><div title="${hover}" style="${styles[1]}">${x}</div></div>`;
}

// TODO: allow this to be controlled via R so not hardcoded names
document.addEventListener(
    'DOMContentLoaded', 
    () => add_truncate_listeners(['name', 'fcst_zone', 'nearest_community'])
);
