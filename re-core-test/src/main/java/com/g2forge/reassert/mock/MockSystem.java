package com.g2forge.reassert.mock;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.system.IRepository;
import com.g2forge.reassert.core.api.system.ISystem;

import lombok.EqualsAndHashCode;
import lombok.ToString;

@ToString
@EqualsAndHashCode
public class MockSystem implements ISystem<MockCoordinates>, ISingleton {
	protected static final MockSystem INSTANCE = new MockSystem();

	public static MockSystem create() {
		return INSTANCE;
	}

	protected MockSystem() {}

	@Override
	public IDescriber<MockCoordinates> getCoordinateDescriber() {
		return MockCoordinatesDescriber.create();
	}

	@Override
	public IRepository<MockCoordinates> getRepository() {
		return null;
	}

	@Override
	public IScanner getScanner() {
		return null;
	}

	@Override
	public MockCoordinates withSystem(MockCoordinates coordinates) {
		if (!isValid(coordinates)) throw new IllegalArgumentException();
		return coordinates;
	}
}
