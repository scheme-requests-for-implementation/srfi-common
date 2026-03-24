!function(s, k, mq, r) {
  var p = s.getItem(k), v = [null, 'light', 'dark'],
      S = '<svg viewBox="0 0 20 20" width="1em" height="1em">',
      C = '<circle cx="10" cy="10" r="9" ',
      c = [S + C + 'fill="none" stroke="currentColor" stroke-width="2"/><path d="M10 1A9 9 0 0 0 10 19Z" fill="currentColor"/></svg>',
           S + C + 'fill="none" stroke="currentColor" stroke-width="2"/></svg>',
           S + C + 'fill="black" stroke="white" stroke-width="2"/></svg>'];
  function a(t) { r.dataset.theme = t || (mq.matches ? 'dark' : 'light') }
  a(p);
  mq.onchange = function() { s.getItem(k) || a() };
  document.addEventListener('DOMContentLoaded', function() {
    var i = Math.max(0, v.indexOf(p)),
        b = document.body.appendChild(document.createElement('button'));
    b.id = 'theme-toggle';
    function u() { b.innerHTML = c[i]; b.title = (v[i] || 'system') + ' theme' }
    b.onclick = function() { i = (i + 1) % 3; v[i] ? s.setItem(k, v[i]) : s.removeItem(k); a(v[i]); u() };
    u()
  })
}(localStorage, 'theme', matchMedia('(prefers-color-scheme: dark)'), document.documentElement);
