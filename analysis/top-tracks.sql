.separator " - "

SELECT COUNT(track.id), artist.name, album.name, title.name FROM scrobble
JOIN track ON scrobble.track_id = track.id
JOIN title ON track.title_id = title.id
JOIN artist ON track.artist_id = artist.id
JOIN album ON track.album_id = album.id
GROUP BY track.id
ORDER BY COUNT(track.id) ASC
;
