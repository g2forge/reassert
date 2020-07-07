package com.g2forge.reassert.list;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.system.IRepository;
import com.g2forge.reassert.core.api.system.ISystem;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.ToString;

@ToString
@EqualsAndHashCode
public class ListSystem implements ISystem<ListCoordinates>, ISingleton {
	protected static final ListSystem INSTANCE = new ListSystem();

	public static ListSystem create() {
		return INSTANCE;
	}

	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	@Getter
	protected final ITypeRef<ListCoordinates> coordinateType = ITypeRef.of(ListCoordinates.class);

	protected ListSystem() {}

	@Override
	public IDescriber<ListCoordinates> getCoordinateDescriber() {
		throw new UnsupportedOperationException();
	}

	@Override
	public IRepository<ListCoordinates> getRepository() {
		throw new UnsupportedOperationException();
	}

	@Override
	public IScanner getScanner() {
		return null;
	}

	@Override
	public ListCoordinates withSystem(ListCoordinates coordinates) {
		if (!isValid(coordinates)) throw new IllegalArgumentException();
		return coordinates;
	}
}
