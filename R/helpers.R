make_summary_chunk <- function(contents) {
  template <- ':::: card

::::: card-body

<details>
<summary>Click for an automated text summary.</summary>

%s
  
</details>

:::::

::::'
  template |> sprintf(contents)
}
