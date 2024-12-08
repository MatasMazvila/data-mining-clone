import requests
import os
from tqdm.notebook import tqdm

from settings import API_KEY, PUBLISHED_AFTER, PUBLISHED_BEFORE, Cache


def handle_api_response(response):
    if response.status_code == 403:
        reason = response.json()["error"]["errors"][0]["reason"]
        if reason == "commentsDisabled":
            return None
    if response.status_code != 200:
        error_message = response.json().get("error", {}).get("message", "Unknown error")
        raise Exception(f"API Error: {error_message}")
    return response.json()


def get_video_categories():
    url = "https://youtube.googleapis.com/youtube/v3/videoCategories"
    params = {"part": "snippet", "hl": "lt", "regionCode": "LT", "key": API_KEY}
    headers = {"Accept": "application/json"}
    response = requests.get(url, headers=headers, params=params)
    response_json = handle_api_response(response)

    # Create a dictionary mapping category IDs to titles
    category_dict = {}
    items = response_json.get("items", [])
    for item in items:
        category_id = item["id"]
        category_title = item["snippet"]["title"]
        category_dict[category_id] = category_title

    return category_dict


def search_videos(q="seimas", maxResults=50):
    url = "https://youtube.googleapis.com/youtube/v3/search"
    params = {
        "part": "id,snippet",
        "q": q,
        "type": "video",
        "publishedAfter": PUBLISHED_AFTER,
        "publishedBefore": PUBLISHED_BEFORE,
        "key": API_KEY,
        "maxResults": maxResults,
        "regionCode": "LT",
        "relevanceLanguage": "LT",
        # 'location': '55.1694,23.8813', # optionally set coordinates
        # 'locationRadius': '200km', # and the radius around them
    }
    headers = {"Accept": "application/json"}
    response = requests.get(url, headers=headers, params=params)
    return handle_api_response(response)


def search_videos_get_channels(q):
    with Cache("search_video_channels", q) as cache_obj:
        if cache_obj.cached:
            return set(cache_obj.data)
        search_results = search_videos(q=q, maxResults=50)
        items = search_results.get("items", [])
        channel_ids = set()
        for item in items:
            channel_id = item["snippet"]["channelId"]
            channel_ids.add(channel_id)

        cache_obj.data = list(channel_ids)
        return cache_obj.data if cache_obj.data else []


def get_channel_info(channel_id):
    with Cache("channel_info", channel_id) as cache_obj:
        if cache_obj.cached:
            return cache_obj.data
        else:
            url = "https://youtube.googleapis.com/youtube/v3/channels"
            params = {
                "part": "snippet,contentDetails,statistics,topicDetails",
                "id": channel_id,
                "key": API_KEY,
            }
            headers = {"Accept": "application/json"}
            response = requests.get(url, headers=headers, params=params)
            response_json = handle_api_response(response)
            cache_obj.data = response_json
            return cache_obj.data


def parse_channel_info(item):
    snippet = item.get("snippet", {})
    statistics = item.get("statistics", {})
    topic_details = item.get("topicDetails", {})
    return {
        "channelId": item.get("id"),
        "title": snippet.get("title"),
        "description": snippet.get("description"),
        "publishedAt": snippet.get("publishedAt"),
        "viewCount": statistics.get("viewCount"),
        "subscriberCount": statistics.get("subscriberCount"),
        "videoCount": statistics.get("videoCount"),
        "topicCategories": topic_details.get("topicCategories", []),
        "country": snippet.get("country"),
    }


def get_channel_videos(channel_id):
    date_range = f"{PUBLISHED_AFTER[:10]}_{PUBLISHED_BEFORE[:10]}"
    cache_dir_name = os.path.join("channel_videos", date_range)
    with Cache(cache_dir_name, channel_id) as cache_obj:
        if cache_obj.cached:
            return cache_obj.data
        else:
            video_ids = []
            page_token = None
            with tqdm(
                desc=f"Fetching videos for channel {channel_id}", leave=False
            ) as pbar:
                while True:
                    url = "https://youtube.googleapis.com/youtube/v3/search"
                    params = {
                        "part": "id",
                        "channelId": channel_id,
                        "type": "video",
                        "publishedAfter": PUBLISHED_AFTER,
                        "publishedBefore": PUBLISHED_BEFORE,
                        "key": API_KEY,
                        "maxResults": 50,
                    }
                    if page_token:
                        params["pageToken"] = page_token
                    headers = {"Accept": "application/json"}
                    response = requests.get(url, headers=headers, params=params)
                    response_json = handle_api_response(response)
                    if response_json is None:
                        break  # Exit loop on error
                    items = response_json.get("items", [])
                    for item in items:
                        video_id = item["id"].get("videoId")
                        if video_id:
                            video_ids.append(video_id)
                    pbar.update(len(items))
                    page_token = response_json.get("nextPageToken")
                    if not page_token:
                        break
            if video_ids:
                cache_obj.data = video_ids
            else:
                cache_obj.data = []
                print(f"No videos found or an error occurred for channel {channel_id}.")
        return cache_obj.data if cache_obj.data else []


def get_videos_stats(video_ids):
    all_video_stats = []
    for video_id in tqdm(video_ids, desc="Fetching video stats", leave=False):
        with Cache("videos_stats", video_id) as cache_obj:
            if cache_obj.cached:
                all_video_stats.append(cache_obj.data)
            else:
                url = "https://youtube.googleapis.com/youtube/v3/videos"
                params = {
                    "part": "id,snippet,statistics",
                    "id": video_id,
                    "key": API_KEY,
                }
                headers = {"Accept": "application/json"}
                response = requests.get(url, headers=headers, params=params)
                response_json = handle_api_response(response)
                if response_json is None:
                    continue  # Skip if there is an error
                items = response_json.get("items", [])
                if not items:
                    continue

                item = items[0]
                stats = {
                    "videoId": item["id"],
                    "channelId": item["snippet"]["channelId"],
                    "viewCount": item["statistics"].get("viewCount"),
                    "likeCount": item["statistics"].get("likeCount"),
                    "commentCount": item["statistics"].get("commentCount"),
                    "categoryId": item["snippet"].get("categoryId"),
                    "publishedAt": item["snippet"].get("publishedAt"),
                }
                cache_obj.data = stats
                all_video_stats.append(stats)

    return all_video_stats


def get_video_comments_inner(video_id, page_token=None):
    url = "https://youtube.googleapis.com/youtube/v3/commentThreads"
    params = {"part": "snippet", "videoId": video_id, "key": API_KEY, "maxResults": 100}
    if page_token:
        params["pageToken"] = page_token
    headers = {"Accept": "application/json"}
    response = requests.get(url, headers=headers, params=params)
    return handle_api_response(response)


def get_video_commenters(video_id):
    with Cache("video_comments", video_id) as cache_obj:
        if cache_obj.cached:
            return set(cache_obj.data)
        else:
            commenters = set()
            page_token = None
            while True:
                response_json = get_video_comments_inner(video_id, page_token)
                if response_json is None:
                    break  # Exit loop on error
                items = response_json.get("items", [])
                for item in items:
                    snippet = (
                        item.get("snippet", {})
                        .get("topLevelComment", {})
                        .get("snippet", {})
                    )
                    author_channel_id = snippet.get("authorChannelId", {}).get("value")
                    if author_channel_id:
                        commenters.add(author_channel_id)
                page_token = response_json.get("nextPageToken")
                if not page_token:
                    break
            cache_obj.data = list(commenters)
        return commenters


SUBSCRIPTION_PASS_ERRORS = [
    "API Error: Subscriptions could not be retrieved because the subscriber's account is suspended.",
    "API Error: The requester is not allowed to access the requested subscriptions.",
    "API Error: Subscriptions could not be retrieved because the subscriber's account is closed.",
]


def get_subscribed_channels(channel_id):
    with Cache("subscriptions", channel_id) as cache_obj:
        if cache_obj.cached:
            return cache_obj.data
        else:
            subscribed_channels = []
            page_token = None
            while True:
                url = "https://youtube.googleapis.com/youtube/v3/subscriptions"
                params = {
                    "part": "snippet",
                    "channelId": channel_id,
                    "key": API_KEY,
                    "maxResults": 50,
                }
                if page_token:
                    params["pageToken"] = page_token
                headers = {"Accept": "application/json"}
                response = requests.get(url, headers=headers, params=params)
                try:
                    response_json = handle_api_response(response)
                except Exception as e:
                    if str(e) not in SUBSCRIPTION_PASS_ERRORS:
                        raise e
                    cache_obj.data = []
                    return []

                # Extract subscribed channel IDs
                items = response_json.get("items", [])
                for item in items:
                    snippet = item.get("snippet", {})
                    subscribed_channel_id = snippet.get("resourceId", {}).get(
                        "channelId"
                    )
                    if subscribed_channel_id:
                        subscribed_channels.append(subscribed_channel_id)

                # Check if there's another page of subscriptions
                page_token = response_json.get("nextPageToken")
                if not page_token:
                    break

            # Cache the subscribed channels list
            cache_obj.data = subscribed_channels
            return cache_obj.data
