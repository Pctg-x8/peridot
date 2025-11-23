#![allow(non_upper_case_globals)]

pub type spa_param_profile = u32;
pub const SPA_PARAM_PROFILE_START: spa_param_profile = 0;
pub const SPA_PARAM_PROFILE_index: spa_param_profile = 1;
pub const SPA_PARAM_PROFILE_name: spa_param_profile = 2;
pub const SPA_PARAM_PROFILE_description: spa_param_profile = 3;
pub const SPA_PARAM_PROFILE_priority: spa_param_profile = 4;
pub const SPA_PARAM_PROFILE_available: spa_param_profile = 5;
pub const SPA_PARAM_PROFILE_info: spa_param_profile = 6;
pub const SPA_PARAM_PROFILE_classes: spa_param_profile = 7;
pub const SPA_PARAM_PROFILE_save: spa_param_profile = 8;

pub type spa_param_route = u32;
pub const SPA_PARAM_ROUTE_START: spa_param_route = 0;
pub const SPA_PARAM_ROUTE_index: spa_param_route = 1;
pub const SPA_PARAM_ROUTE_direction: spa_param_route = 2;
pub const SPA_PARAM_ROUTE_device: spa_param_route = 3;
pub const SPA_PARAM_ROUTE_name: spa_param_route = 4;
pub const SPA_PARAM_ROUTE_description: spa_param_route = 5;
pub const SPA_PARAM_ROUTE_priority: spa_param_route = 6;
pub const SPA_PARAM_ROUTE_available: spa_param_route = 7;
pub const SPA_PARAM_ROUTE_info: spa_param_route = 8;
pub const SPA_PARAM_ROUTE_profiles: spa_param_route = 9;
pub const SPA_PARAM_ROUTE_props: spa_param_route = 10;
pub const SPA_PARAM_ROUTE_devices: spa_param_route = 11;
pub const SPA_PARAM_ROUTE_profile: spa_param_route = 12;
pub const SPA_PARAM_ROUTE_save: spa_param_route = 13;

#[repr(C)]
pub enum spa_media_type {
    unknown,
    audio,
    video,
    image,
    binary,
    stream,
    application,
}

#[repr(C)]
pub enum spa_media_subtype {
    unknown,
    raw,
    dsp,
    iec958,
    dsd,
    START_Audio = 0x10000,
    mp3,
    aac,
    vorbis,
    wma,
    ra,
    sbc,
    adpcm,
    g723,
    g726,
    g729,
    amr,
    gsm,
    alac,
    flac,
    ape,
    opus,
    START_Video = 0x20000,
    h264,
    mjpg,
    dv,
    mpegts,
    h263,
    mpeg1,
    mpeg2,
    mpeg4,
    xvid,
    vc1,
    vp8,
    vp9,
    bayer,
    START_Image = 0x30000,
    jpeg,
    START_Binary = 0x40000,
    START_Stream = 0x50000,
    midi,
    START_Application = 0x60000,
    control,
}

#[repr(C)]
pub enum spa_format {
    mediaType = 1,
    mediaSubtype,
    START_Audio = 0x10000,
    AUDIO_format,
    AUDIO_flags,
    AUDIO_rate,
    AUDIO_channels,
    AUDIO_position,
    AUDIO_iec958Codec,
    AUDIO_bitorder,
    AUDIO_interleave,
    AUDIO_bitrate,
    AUDIO_blockAlign,
    AUDIO_ACC_streamFormat,
    AUDIO_WMA_profile,
    AUDIO_AMR_bandMode,
    START_Video = 0x20000,
    VIDEO_format,
    VIDEO_modifier,
    VIDEO_size,
    VIDEO_framerate,
    VIDEO_maxFramerate,
    VIDEO_views,
    VIDEO_interlaceMode,
    VIDEO_pixelAspectRAtio,
    VIDEO_multiviewMode,
    VIDEO_multiviewFlags,
    VIDEO_chromaSite,
    VIDEO_colorRange,
    VIDEO_colorMatrix,
    VIDEO_transferFunction,
    VIDEO_colorPrimaries,
    VIDEO_profile,
    VIDEO_level,
    VIDEO_H264_streamFormat,
    VIDEO_H264_alignment,
    START_Image = 0x30000,
    START_Binary = 0x40000,
    START_Stream = 0x50000,
    START_Application = 0x60000,
    CONTROL_types,
}

#[repr(C)]
pub enum spa_prop_info {
    id = 1,
    name = 2,
    r#type = 3,
    labels = 4,
    container = 5,
    params = 6,
    description = 7,
}

#[repr(C)]
pub enum spa_prop {
    unknown = 1,
    START_Device = 0x100,
    device,
    deviceName,
    deviceFd,
    card,
    cardName,
    minLatency,
    maxLatency,
    periods,
    periodSize,
    periodEvent,
    live,
    rate,
    quality,
    bluetoothAudioCodec,
    bluetoothOffloadActive,
    START_Audio = 0x10000,
    waveType,
    frequency,
    volume,
    mute,
    patternType,
    ditherType,
    truncate,
    channelVolumes,
    volumeBase,
    volumeStep,
    channelMap,
    monitorMute,
    monitorVolumes,
    latencyOffsetNsec,
    softMute,
    softVolumes,
    iec958Codecs,
    volumeRampSamples,
    volumeRampStepSamples,
    volumeRampTime,
    volumeRampStepTime,
    volumeRampScale,
    START_Video = 0x20000,
    brightness,
    contrast,
    saturation,
    hue,
    gamma,
    exposure,
    gain,
    sharpness,
    START_Other = 0x80000,
    params,
}

#[repr(C)]
pub enum spa_param_port_config_mode {
    none,
    pasthrough,
    convert,
    dsp,
}

#[repr(C)]
pub enum spa_param_port_config {
    direction = 1,
    mode,
    monitor,
    control,
    format,
}

#[repr(C)]
pub enum spa_param_latency {
    direction = 1,
    minQuantum,
    maxQuantum,
    minRate,
    maxRate,
    minNs,
    maxNs,
}

mod audio;
pub use self::audio::*;
