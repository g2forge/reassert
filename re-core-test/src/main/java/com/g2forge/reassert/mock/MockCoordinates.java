package com.g2forge.reassert.mock;

import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class MockCoordinates implements ICoordinates {
	protected final String text;

	@Override
	public MockSystem getSystem() {
		return MockSystem.create();
	}
}
