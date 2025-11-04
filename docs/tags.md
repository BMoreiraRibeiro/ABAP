# 📚 Índice de Tags

{% for tag in tags %}
- [{{ tag.name }}]({{ tag.url }}) — {{ tag.count }} páginas
{% endfor %}
