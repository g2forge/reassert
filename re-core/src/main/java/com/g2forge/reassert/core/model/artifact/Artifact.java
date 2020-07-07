package com.g2forge.reassert.core.model.artifact;

import com.g2forge.reassert.core.api.system.IRepository;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Artifact<Coordinates extends ICoordinates> implements IVertex {
	protected final IRepository<Coordinates> repository;

	protected final Coordinates coordinates;

	@Override
	public boolean isMaterial() {
		return true;
	}
}
