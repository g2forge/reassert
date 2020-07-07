package com.g2forge.reassert.list.model;

import java.util.Map;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class StoredGraph {
	@Singular("vertex")
	protected final Map<String, StoredVertex> vertices;
}