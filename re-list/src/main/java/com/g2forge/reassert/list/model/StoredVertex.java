package com.g2forge.reassert.list.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.g2forge.reassert.core.model.IVertex;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class StoredVertex {
	protected final IVertex vertex;

	@Singular
	@JsonInclude(JsonInclude.Include.NON_EMPTY)
	protected final List<StoredEdge> outgoings;
}