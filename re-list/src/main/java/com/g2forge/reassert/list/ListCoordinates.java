package com.g2forge.reassert.list;

import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ListCoordinates implements ICoordinates {
	protected final IDataSource source;

	@Override
	public ListSystem getSystem() {
		return ListSystem.create();
	}
}
