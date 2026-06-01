extends Node

@onready var http_request: HTTPRequest = $HTTPRequest

var selected_id: String = ""
var enemies_alive = 0
var hits_taken = 0
var enemies_defeated = 0

var supabase_api_key = ""
var supabase_url = "https://tajerjmvchsmddhmxwmj.supabase.co/rest/v1/"
var headers = [
				"Content-Type: application/json", 
			  	"apikey: sb_publishable_-Ap2Oix5EPoCCafGTGL7eg_7jxcc7Zk",
				"Authorization: Bearer sb_publishable_-Ap2Oix5EPoCCafGTGL7eg_7jxcc7Zk",
				"Prefer: return=minimal"
				]

func add_game_data(test: Dictionary):
	var data = JSON.stringify(test)
	var url = supabase_url + "game_data"
	http_request.request(url, headers, HTTPClient.Method.METHOD_POST, data)
	pass
#curl "http://localhost:3000/table_name" \
  #-X POST -H "Content-Type: application/json" \
  #-d '{ "col1": "value1", "col2": "value2" }'
