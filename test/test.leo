todos = [
  "Watch Iron Man",
  "Travel to France",
  "Eat a croissant"
]

line x =
  <li>@x</li>

<html><head>
</head><body>
<h1>Bucket list</h1>
<ul>@(List.map
 line todos)</ul>
</body>
</html>