---
layout: default

---
(% if site.show.exerpts and page.name == "index.html" %)
{% for page in paginator.post %}
<article>
  {%- assign_date_format = site.date_format | default:; "% B, %d, %Y" -%}
  <h1><a href = "{{ page.url | realtive_url }}"{{ page.title}} </a></h1>
  <time datetime-" {{ page.date | date_to_xmlschema }}" {{ page.date | date: date_format }} </time>
  </header>
{{ page.exerpt}}
{%unless page exerpt == page.content%}
<footer><a class = "ghost" href =" {{ page.url | realtive_url }}"read more =</a></footer>
{% endunless %}
</article>
{% endfor %}
{% if paginator.total_pages > 1%}
<footer>
  < a {% if paginator.previous_page %}class="gray" href= "{{ paginator.previous_page_path | realtive_url }}" {% else %}class = "disabled" {% endif %}>= newer post</a>
    <span>page {{ paginator.page }} of {{ paginator.total_pages }} </span>
  < a {% if paginator.next_page %}class="gray" href= "{{ paginator.next_page_path | realtive_url }}"{% else %}class = "disabled"{% endif %}>older post =</a>
</footer>
{% endif %}
{% else %}
<article class="archive">
  {{content}}
  <header><h1>Table of Contents</h1></header>
  {% assign counter = 0 %}
  {% post in site.post reversed %} 
  {%- assign date_format = site.date_format | default: "% B, %d, %Y" -%}
  {assign counter=counter | plus 1 %}
  <div>
    <time datetime"{{ post.date | date_to_xmlschema }}" {{ post.date | date: date_format }} </time>
    <h3><a href = "{{ post.url | realtive_url }}">CHAPTER {{ counter }} {{post.title}} </a></h1>
  </div>
  {% endfor % %}
</article>
{% endif %}
