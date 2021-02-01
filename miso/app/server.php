<?php

header('Access-Control-Allow-Origin: *');
header('Content-Type: application/json');

$requestUri = $_SERVER['REQUEST_URI'];
if ($requestUri === '/album/1') {
    echo file_get_contents(__DIR__ . '/album.json');
}
