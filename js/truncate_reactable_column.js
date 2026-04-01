function add_truncate_listeners(columns) {
    let all_reactables = document.getElementsByClassName("reactable");
    Array.from(all_reactables).forEach(
        elem => Reactable.onStateChange(elem.id, state => truncate_reactable_columns(elem.id, columns))
    );
}

function truncate_reactable_columns(table_id, columns) {
    const tbl_cols = get_reactable_columns(table_id);
    for (let j = 0; j < columns.length; j++) {
        let header_label = columns[j];
        let is_desired_col = tbl_cols.map(x => x === header_label);
        is_desired_col.forEach((test, index) => { if (test) truncate_reactable_column(table_id, index) });
    }
};

function get_reactable_columns(table_id) {
    const table = document.getElementById(table_id);
    const headers = table.querySelectorAll(".rt-tr-header .rt-th");
    return [...headers].map(header => header.innerText);
}

function truncate_reactable_column(table_id, header_index) {
    const table = document.getElementById(table_id);
    const rows = table.querySelectorAll(".rt-tbody .rt-tr-group .rt-tr");
    rows.forEach((row) => {
        const cell = row.childNodes[header_index].querySelector(".rt-text-content");
        cell.innerHTML = truncate_long_text(cell.innerHTML);
    });
}

function truncate_long_text(x) {
    let xSafe = x.replace(/'/g, "&rsquo;").replace(/"/g, "&quot;");
    let styles = ["display:table; table-layout:fixed; width:100%", "overflow-x:hidden; text-overflow:ellipsis; white-space:nowrap"];
    return `<div style="${styles[0]}"><p title="${xSafe}" style="${styles[1]}">${x}</p></div>`;
}

// TODO: allow this to be controlled via R so not hardcoded names
document.addEventListener('DOMContentLoaded', function() {
  add_truncate_listeners(['Name', 'Region']);
});
