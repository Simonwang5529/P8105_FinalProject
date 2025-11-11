// Mobile menu
const toggle = document.querySelector('.menu-toggle');
const nav = document.getElementById('main-nav');
if (toggle && nav) {
  toggle.addEventListener('click', () => {
    const open = nav.classList.toggle('open');
    toggle.setAttribute('aria-expanded', String(open));
  });
}

// Active link styling by URL
const setActive = () => {
  const here = location.pathname.split('/').pop() || 'index.html';
  document.querySelectorAll('.nav a').forEach(a => {
    const href = a.getAttribute('href');
    if (!href) return;
    if (href === here) a.setAttribute('aria-current', 'page');
  });
};
setActive();

// Close menu on outside click (mobile)
document.addEventListener('click', (e) => {
  if (!nav) return;
  if (!nav.classList.contains('open')) return;
  const within = nav.contains(e.target) || (toggle && toggle.contains(e.target));
  if (!within) nav.classList.remove('open');
});
