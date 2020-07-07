package com.g2forge.reassert.list.model;

import java.util.List;

import com.g2forge.reassert.core.model.IEdge;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class StoredEdge {
	protected final IEdge edge;

	@Singular
	protected final List<String> targets;
}