[
  {"selector":"node", "css": {
    "content": "data(id)",
    "text-valign":"center",
    "text-halign":"center",
    "width": "mapData(scaled_size, 0, 1, 10, 100)",
    "height": "mapData(scaled_size, 0, 1, 10, 100)",
    "border-width": "1px"
  }},
  
  {"selector": "node[color<=0]", "css": {
  	"background-color": "mapData(color, -1, 0, #4682B4, #F5F5F5)"
   }},

  {"selector": "node[color>0]", "css": {
		"background-color": "mapData(color, 0, 2, #F5F5F5, #B22222)"
  }},
  
  {"selector": "node:selected", "css": {
    "overlay-opacity": 0.3,
    "overlay-color": "gray"
  }},
  
  {"selector": "edge[interaction='stimulate']", "css": {
    "line-color": "lightgrey",
    "line-width": "1px"
  }},
  {"selector": "edge[interaction='inhibit']", "css": {
    "line-color": "lightgrey"
  }}
]