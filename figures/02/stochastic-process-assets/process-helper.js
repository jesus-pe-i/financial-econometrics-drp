(function() {
  var script = document.currentScript;
  var base = script.src.replace(/[^/]*$/, '');
  window.process02 = {
    base: base,
    readMatrix: async function(file, nrow, ncol) {
      var buf = await fetch(base + file).then(function(r) {
        if (!r.ok) throw new Error('Failed to load ' + file);
        return r.arrayBuffer();
      });
      var values = new Float32Array(buf);
      var out = new Array(ncol);
      for (var j = 0; j < ncol; j++) {
        out[j] = Array.from(values.slice(j * nrow, (j + 1) * nrow));
      }
      return out;
    }
  };
})();
