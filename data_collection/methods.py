import pandas as pd
from collections import defaultdict
from itertools import combinations
from concurrent.futures import ThreadPoolExecutor
from tqdm.notebook import tqdm

from api import *
from settings import (
    THRESHOLD_CHANNEL_SUBS,
    THRESHOLD_CHANNEL_VIEWS,
    THRESHOLD_VIDEO_VIEWS,
    CACHE_DIR,
    THRESHOLD_PERIOD_VIEWS,
    THRESHOLD_PERIOD_COMMENTS,
    CATEGORIES,
)


def test_api():
    try:
        categories = get_video_categories()
    except:
        return False
    return categories != None


def get_channels_metadata(channel_ids):
    channel_infos = []
    for channel_id in tqdm(channel_ids):
        info_response = get_channel_info(channel_id)
        items_info = info_response.get("items", [])
        if items_info:
            info = parse_channel_info(items_info[0])
            channel_infos.append(info)
    df_channels = pd.DataFrame(channel_infos)
    # Convert numeric columns to integers
    numeric_columns = ["viewCount", "subscriberCount", "videoCount"]
    df_channels[numeric_columns] = (
        df_channels[numeric_columns].astype(float).fillna(0).astype(int)
    )
    df_channels = df_channels.set_index("channelId")
    return df_channels


def filter_channels(df_channels):
    df_channels = df_channels.query(
        f"country == 'LT' and "
        f"subscriberCount > {THRESHOLD_CHANNEL_SUBS} and "
        f"viewCount > {THRESHOLD_CHANNEL_VIEWS}"
    )
    return df_channels


def get_all_search_channels():
    channel_dir = os.path.join(CACHE_DIR, "search_video_channels")
    file_names = os.listdir(channel_dir)
    channel_ids = []
    for n in file_names:
        with open(os.path.join(channel_dir, n), "r") as f:
            data = json.load(f)
        channel_ids += data
    return list(set(channel_ids))


def get_all_channels():
    channel_dir = os.path.join(CACHE_DIR, "channel_info")
    file_names = os.listdir(channel_dir)
    channel_ids = [f.split(".")[0] for f in file_names]
    return channel_ids


def get_all_videos(channel_ids):
    video_ids = []
    for channel_id in tqdm(channel_ids):
        ids = get_channel_videos(channel_id)
        video_ids.extend(ids)
    video_stats = get_videos_stats(video_ids)
    df_videos = pd.DataFrame(video_stats)

    # Convert numeric columns to integers
    numeric_columns = ["viewCount", "likeCount", "commentCount"]
    df_videos[numeric_columns] = (
        df_videos[numeric_columns].astype(float).fillna(0).astype(int)
    )
    return df_videos


def filter_videos(videos):
    # Filter videos based on view count threshold
    videos = videos.query(
        f"viewCount > {THRESHOLD_VIDEO_VIEWS} and " "commentCount > 0"
    )
    return videos


def aggregate_and_filter_videos(videos):
    def most_common(series):
        return series.mode().values[0]

    channel_aggregates = videos.groupby("channelId").agg(
        period_views=("viewCount", "sum"),
        period_comments=("commentCount", "sum"),
        period_video_count=("videoId", "count"),
        most_common_video_category=("categoryId", most_common),
        video_ids=("videoId", list),
    )
    channel_aggregates["most_common_video_category_name"] = (
        channel_aggregates.most_common_video_category.apply(
            lambda x: CATEGORIES[str(x)] if str(x) in CATEGORIES else "Unknown"
        )
    )
    channel_aggregates = channel_aggregates.query(
        f"period_views > {THRESHOLD_PERIOD_VIEWS} and "
        f"period_comments> {THRESHOLD_PERIOD_COMMENTS}"
    )
    return channel_aggregates


def get_channel_commenters(row):
    if row["commenters"]:
        return row["commenters"]

    video_ids = row["video_ids"]
    commenters = set()
    pbar = tqdm(total=len(video_ids), leave=False, position=1)
    for video_id in video_ids:
        pbar.set_description(f"Fetching commenters for channel {row['title']}")
        video_commenters = get_video_commenters(video_id)
        commenters.update(video_commenters)
        pbar.update(1)
    pbar.close()
    return commenters


def get_jaccard_index(list1, list2):
    set1 = set(list1)
    set2 = set(list2)
    intersection = set1.intersection(set2)
    union = set1.union(set2)
    if union:
        jaccard_index = len(intersection) / len(union)
    else:
        jaccard_index = 0.0
    return jaccard_index


def get_overlap_coefficient(list1, list2):
    set1 = set(list1)
    set2 = set(list2)
    intersection = set1.intersection(set2)
    min_len = min(len(set1), len(set2))
    overlap_coefficient = len(intersection) / min_len
    return overlap_coefficient


def get_similarity(df_channels, target_col, method="jaccard"):
    if method == "overlap":
        measure_function = get_overlap_coefficient
    else:
        measure_function = get_jaccard_index
    pairs = combinations(df_channels.index, 2)
    matrix = pd.DataFrame(index=df_channels.title, columns=df_channels.title)

    for ch1, ch2 in pairs:
        commenters1 = df_channels.loc[ch1, target_col]
        commenters2 = df_channels.loc[ch2, target_col]
        jaccard_index = measure_function(commenters1, commenters2)
        ch1_title = df_channels.loc[ch1].title
        ch2_title = df_channels.loc[ch2].title
        matrix.at[ch1_title, ch2_title] = jaccard_index
        matrix.at[ch2_title, ch1_title] = jaccard_index

    for ch in df_channels.index:
        ch_title = df_channels.loc[ch].title
        matrix.at[ch_title, ch_title] = 1.0

    return matrix


def cache_subscribers(df_channels):
    pbar = tqdm(total=len(df_channels), position=0)
    for ch, row in df_channels.iterrows():
        pbar.set_description(f"Fetching for channel {row['title']}")
        if not row["subs_processed"]:
            commenters = sorted(list(row["commenters"]))
            commenters = commenters
            pbar2 = tqdm(total=len(commenters), leave=False, position=1)
            for co in commenters:
                cache_obj = Cache("subscriptions", co)
                if not cache_obj.cached:
                    pbar2.set_description("NO CACHE")
                    s = get_subscribed_channels(co)
                else:
                    pbar2.set_description("CACHED")
                pbar2.update(1)
            pbar2.close()

        pbar.update(1)
        df_channels.loc[ch, "subs_processed"] = True


def count_subs_per_channel(df_channels):
    sub_count = defaultdict(int)
    pbar = tqdm(total=len(df_channels))

    def process_channel(row):
        channel_sub_count = defaultdict(int)
        commenters = list(row["commenters"])
        for co in commenters:
            cache_obj = Cache("subscriptions", co)
            if cache_obj.cached:
                subs = get_subscribed_channels(co)
                for s in subs:
                    channel_sub_count[s] += 1
        return channel_sub_count

    def merge_counts(target, source):
        for key, value in source.items():
            target[key] += value

    with ThreadPoolExecutor(max_workers=12) as executor:  # Limit to 12 threads
        futures = {
            executor.submit(process_channel, row): ch
            for ch, row in df_channels.iterrows()
        }
        for future in futures:
            result = future.result()
            merge_counts(sub_count, result)
            pbar.update(1)

    pbar.close()
    return sub_count


def get_subs_per_channel(df_channels):
    allowed_channels = df_channels.index.to_list()
    subs_per_channel = defaultdict(list)
    pbar = tqdm(total=len(df_channels))

    def process_channel(row):
        subs_per_channel = defaultdict(list)
        commenters = list(row["commenters"])
        for co in commenters:
            cache_obj = Cache("subscriptions", co)
            if cache_obj.cached:
                subs = get_subscribed_channels(co)
                subs = list(filter(lambda x: x in allowed_channels, subs))
                for s in subs:
                    subs_per_channel[s].append(co)
        return subs_per_channel

    def merge_counts(target, source):
        for key, value in source.items():
            target[key] += value

    with ThreadPoolExecutor() as executor:
        futures = {
            executor.submit(process_channel, row): ch
            for ch, row in df_channels.iterrows()
        }
        for future in futures:
            result = future.result()
            merge_counts(subs_per_channel, result)
            pbar.update(1)
    pbar.close()
    return subs_per_channel
