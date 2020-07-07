package com.g2forge.reassert.core.model.file;

import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class File implements IVertex {
	protected final ICoordinates coordinates;

	@Override
	public boolean isMaterial() {
		return true;
	}
}
