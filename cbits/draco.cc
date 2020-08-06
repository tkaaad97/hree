#include <iostream>

#include "draco/javascript/emscripten/decoder_webidl_wrapper.cc"

extern "C" {

Decoder* draco_newDecoder() {
    return new Decoder();
}

void draco_deleteDecoder(Decoder* decoder) {
    delete decoder;
}

draco::DecoderBuffer* draco_newDecoderBuffer(const char* data, size_t data_size) {
    draco::DecoderBuffer* decoderBuffer = new draco::DecoderBuffer();
    decoderBuffer->Init(data, data_size);
    return decoderBuffer;
}

void draco_deleteDecoderBuffer(draco::DecoderBuffer* decoderBuffer) {
    delete decoderBuffer;
}

draco::PointCloud* draco_newPointCloud() {
    return new draco::PointCloud();
}

void draco_deletePointCloud(draco::PointCloud* pc) {
    delete pc;
}

draco::Mesh* draco_newMesh() {
    return new draco::Mesh();
}

void draco_deleteMesh(draco::Mesh* mesh) {
    delete mesh;
}

draco_EncodedGeometryType draco_getEncodedGeometryType(Decoder* decoder, DecoderBuffer *in_buffer) {
    return decoder->GetEncodedGeometryType(in_buffer);
}

draco::PointCloud* draco_castMeshToPointCloud(draco::Mesh* mesh) {
    return (draco::PointCloud*)mesh;
}

const draco::Status* draco_decodeBufferToPointCloud(Decoder* decoder, draco::DecoderBuffer *in_buffer, draco::PointCloud *out_point_cloud) {
    return decoder->DecodeBufferToPointCloud(in_buffer, out_point_cloud);
}

const draco::Status* draco_decodeBufferToMesh(Decoder* decoder, draco::DecoderBuffer *in_buffer, draco::Mesh *out_mesh) {
    return decoder->DecodeBufferToMesh(in_buffer, out_mesh);
}

long draco_getAttributeIdByName(const draco::PointCloud* pc, const char* attribute_name) {
    return Decoder::GetAttributeIdByName(*pc, attribute_name);
}

const draco::PointAttribute* draco_getAttribute(const draco::PointCloud* pc, long att_id) {
    return Decoder::GetAttribute(*pc, att_id);
}

const draco::PointAttribute* draco_getAttributeByUniqueId(const draco::PointCloud* pc, long unique_id) {
    return Decoder::GetAttributeByUniqueId(*pc, unique_id);
}

bool draco_getAttributeFloatArrayForAllPoints(
    const draco::PointCloud* pc,
    const draco::PointAttribute* pa,
    int out_size,
    void* out_values) {
    return Decoder::GetAttributeFloatArrayForAllPoints(*pc, *pa, out_size, out_values);
}

bool draco_getAttributeInt8ArrayForAllPoints(
    const draco::PointCloud *pc,
    const draco::PointAttribute *pa,
    int out_size,
    void* out_values) {
    return Decoder::GetAttributeDataArrayForAllPoints(*pc, *pa, draco::DT_INT8, out_size, out_values);
}

bool draco_getAttributeUInt8ArrayForAllPoints(
    const draco::PointCloud* pc,
    const draco::PointAttribute* pa,
    int out_size,
    void* out_values) {
    return Decoder::GetAttributeDataArrayForAllPoints(*pc, *pa, draco::DT_UINT8, out_size, out_values);
}

bool draco_getAttributeInt16ArrayForAllPoints(
    const draco::PointCloud* pc,
    const draco::PointAttribute* pa,
    int out_size,
    void* out_values) {
    return Decoder::GetAttributeDataArrayForAllPoints(*pc, *pa, draco::DT_INT16, out_size, out_values);
}

bool draco_getAttributeUInt16ArrayForAllPoints(
    const draco::PointCloud* pc,
    const draco::PointAttribute* pa,
    int out_size,
    void* out_values) {
    return Decoder::GetAttributeDataArrayForAllPoints(*pc, *pa, draco::DT_UINT16, out_size, out_values);
}

bool draco_getAttributeInt32ArrayForAllPoints(
    const draco::PointCloud* pc,
    const draco::PointAttribute* pa,
    int out_size,
    void* out_values) {
    return Decoder::GetAttributeDataArrayForAllPoints(*pc, *pa, draco::DT_INT32, out_size, out_values);
}

bool draco_getAttributeUInt32ArrayForAllPoints(
    const draco::PointCloud* pc,
    const draco::PointAttribute* pa,
    int out_size,
    void* out_values) {
    return Decoder::GetAttributeDataArrayForAllPoints(*pc, *pa, draco::DT_UINT32, out_size, out_values);
}

size_t draco_getNumFaces(
    const draco::Mesh* mesh) {
    return mesh->num_faces();
}

size_t draco_getIndicesByteSize(
    const draco::Mesh* mesh) {
    size_t num_faces = mesh->num_faces();
    size_t num_indices = num_faces * 3;
    return num_indices * sizeof(uint32_t);
}

bool draco_getIndices(
    const draco::Mesh* mesh,
    int out_size,
    void* out_values) {

    if (out_size != draco_getIndicesByteSize(mesh)) {
        return false;
    }

    size_t num_faces = mesh->num_faces();
    uint32_t* const indices = (uint32_t*)out_values;
    for (size_t i = 0; i < num_faces; ++i) {
        size_t index = i * 3;
        draco::Mesh::Face const& face = mesh->face(draco::FaceIndex(i));
        indices[index] = face[0].value();
        indices[index + 1] = face[1].value();
        indices[index + 2] = face[2].value();
    }

    return true;
}

bool draco_ok(const draco::Status* status) {
    return status->ok();
}

const char* draco_error_msg(const draco::Status* status) {
    return status->error_msg();
}

}
