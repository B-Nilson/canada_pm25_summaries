return {
  ['pm_units'] = function(args, kwargs, meta, raw_args, context) 
    -- see https://quarto.org/docs/extensions/shortcodes.html
    -- for documentation on shortcode development
    return pandoc.RawBlock('html', "&mu;g m<sup>-3</sup>")
  end
}
