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
// script.js

// Run when DOM is ready
document.addEventListener("DOMContentLoaded", function () {
  // 1) Page fade-in
  document.body.classList.add("is-loaded");

  // 2) Smooth fade-out before navigating to another HTML page
  const links = document.querySelectorAll('a[href$=".html"]:not([target="_blank"])');

  links.forEach(link => {
    link.addEventListener("click", function (e) {
      const href = this.getAttribute("href");

      // Ignore anchor-only links and empty hrefs
      if (!href || href.startsWith("#")) return;

      e.preventDefault();

      // Add fade-out class
      document.body.classList.add("is-fading-out");

      // After the transition, go to the new page
      setTimeout(() => {
        window.location.href = href;
      }, 300); // should be a bit less or equal to CSS transition time
    });
  });

  // 3) Mobile menu toggle behavior
  const menuBtn = document.querySelector(".menu-toggle");
  const mainNav = document.getElementById("main-nav");

  if (menuBtn && mainNav) {
    menuBtn.addEventListener("click", () => {
      const expanded = menuBtn.getAttribute("aria-expanded") === "true";
      menuBtn.setAttribute("aria-expanded", String(!expanded));
      mainNav.classList.toggle("is-open");
    });
  }
});
