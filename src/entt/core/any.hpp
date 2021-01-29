#ifndef ENTT_CORE_ANY_HPP
#define ENTT_CORE_ANY_HPP


#include <functional>
#include <new>
#include <type_traits>
#include <utility>
#include "../config/config.h"
#include "../core/utility.hpp"
#include "type_info.hpp"
#include "type_traits.hpp"


namespace entt {


/*! @brief A SBO friendly, type-safe container for single values of any type. */
class any {
    enum class operation { COPY, MOVE, DTOR, COMP, ADDR, CADDR, REF, CREF, TYPE };

    using storage_type = std::aligned_storage_t<sizeof(double[2])>;
    using vtable_type = const void *(const operation, const any &, const void *);

    template<typename Type>
    static constexpr auto is_dyn_v = sizeof(Type) > sizeof(storage_type) || !std::is_nothrow_move_constructible_v<Type>;

    template<typename Type>
    [[nodiscard]] static bool compare(const void *lhs, const void *rhs) {
        if constexpr(!std::is_function_v<Type> && is_equality_comparable_v<Type>) {
            return *static_cast<const Type *>(lhs) == *static_cast<const Type *>(rhs);
        } else {
            return lhs == rhs;
        }
    }

    template<typename Type>
    static Type & as(const void *to) {
        return *const_cast<Type *>(static_cast<const Type *>(to));
    }

    static const void * void_vtable(const operation, const any &, const void *) {
        return nullptr;
    }

    template<typename Type>
    static const void * ref_vtable(const operation op, const any &from, const void *to) {
        switch(op) {
        case operation::COPY:
        case operation::MOVE:
            return (as<any>(to).instance = from.instance);
        case operation::DTOR:
            break;
        case operation::COMP:
            return compare<std::remove_const_t<Type>>(from.instance, to) ? to : nullptr;
        case operation::ADDR:
            return std::is_const_v<Type> ? nullptr : from.instance;
        case operation::CADDR:
            return from.instance;
        case operation::REF:
            as<any>(to).vtable = ref_vtable<Type>;
            as<any>(to).instance = from.instance;
            break;
        case operation::CREF:
            as<any>(to).vtable = ref_vtable<const Type>;
            as<any>(to).instance = from.instance;
            break;
        case operation::TYPE:
            as<type_info>(to) = type_id<std::remove_const_t<Type>>();
            break;
        }

        return nullptr;
    }

    template<typename Type>
    static const void * dyn_vtable(const operation op, const any &from, const void *to) {
        switch(op) {
        case operation::COPY:
            if constexpr(std::is_copy_constructible_v<Type>) {
                return (as<any>(to).instance = new Type{*static_cast<const Type *>(from.instance)});
            }
            break;
        case operation::MOVE:
            as<any>(to).instance = from.instance;
            break;
        case operation::DTOR:
            if constexpr(std::is_array_v<Type>) {
                delete[] static_cast<const Type *>(from.instance);
            } else {
                delete static_cast<const Type *>(from.instance);
            }
            break;
        case operation::COMP:
            return compare<Type>(from.instance, to) ? to : nullptr;
        case operation::ADDR:
        case operation::CADDR:
            return from.instance;
        case operation::REF:
            as<any>(to).vtable = ref_vtable<Type>;
            as<any>(to).instance = from.instance;
            break;
        case operation::CREF:
            as<any>(to).vtable = ref_vtable<const Type>;
            as<any>(to).instance = from.instance;
            break;
        case operation::TYPE:
            as<type_info>(to) = type_id<Type>();
            break;
        }

        return nullptr;
    }

    template<typename Type>
    static const void * sbo_vtable(const operation op, const any &from, const void *to) {
        #if __cpp_lib_launder >= 201606L
        auto *instance = const_cast<Type *>(std::launder(reinterpret_cast<const Type *>(&from.storage)));
        #else
        auto *instance = const_cast<Type *>(reinterpret_cast<const Type *>(&from.storage));
        #endif

        switch(op) {
        case operation::COPY:
            if constexpr(std::is_copy_constructible_v<Type>) {
                return new (&as<any>(to).storage) Type{std::as_const(*instance)};
            }
            break;
        case operation::MOVE:
            new (&as<any>(to).storage) Type{std::move(*instance)};
            [[fallthrough]];
        case operation::DTOR:
            instance->~Type();
            break;
        case operation::COMP:
            return compare<Type>(instance, to) ? to : nullptr;
        case operation::ADDR:
        case operation::CADDR:
            return instance;
        case operation::REF:
            as<any>(to).vtable = ref_vtable<Type>;
            as<any>(to).instance = instance;
            break;
        case operation::CREF:
            as<any>(to).vtable = ref_vtable<const Type>;
            as<any>(to).instance = instance;
            break;
        case operation::TYPE:
            as<type_info>(to) = type_id<Type>();
            break;
        }

        return nullptr;
    }

public:
    /*! @brief Default constructor. */
    any() ENTT_NOEXCEPT
        : any{in_place_type<void>}
    {}

    /**
     * @brief Constructs an any by directly initializing the new object.
     *
     * This overload ensures that neither aliasing nor small object optimization
     * are used. The object will therefore be created dynamically.
     *
     * @tparam Type Type of object to use to initialize the wrapper.
     * @tparam Args Types of arguments to use to construct the new instance.
     * @param args Parameters to use to construct the instance.
     */
    template<typename Type, typename... Args>
    explicit any(in_place_type_dyn_t<Type>, [[maybe_unused]] Args &&... args)
        : vtable{&void_vtable},
          instance{}
    {
        if constexpr(!std::is_void_v<Type>) {
            vtable = &dyn_vtable<std::remove_reference_t<Type>>;
            instance = new std::remove_reference_t<Type>(std::forward<Args>(args)...);
        }
    }

    /**
     * @brief Constructs an any by directly initializing the new object.
     * @tparam Type Type of object to use to initialize the wrapper.
     * @tparam Args Types of arguments to use to construct the new instance.
     * @param args Parameters to use to construct the instance.
     */
    template<typename Type, typename... Args>
    explicit any(in_place_type_t<Type>, [[maybe_unused]] Args &&... args)
        : vtable{&void_vtable},
          instance{}
    {
        if constexpr(!std::is_void_v<Type>) {
            if constexpr(std::is_lvalue_reference_v<Type>) {
                static_assert(sizeof...(Args) == 1u && (std::is_lvalue_reference_v<Args> && ...));
                vtable = &ref_vtable<std::remove_reference_t<Type>>;
                instance = (&args, ...);
            } else if constexpr(is_dyn_v<Type>) {
                vtable = &dyn_vtable<Type>;
                instance = new Type(std::forward<Args>(args)...);
            } else {
                vtable = &sbo_vtable<Type>;
                new (&storage) Type(std::forward<Args>(args)...);
            }
        }
    }

    /**
     * @brief Constructs an any that holds an unmanaged object.
     * @tparam Type Type of object to use to initialize the wrapper.
     * @param value An instance of an object to use to initialize the wrapper.
     */
    template<typename Type>
    any(std::reference_wrapper<Type> value) ENTT_NOEXCEPT
        : any{in_place_type<Type &>, value.get()}
    {}

    /**
     * @brief Constructs an any from a given value.
     * @tparam Type Type of object to use to initialize the wrapper.
     * @param value An instance of an object to use to initialize the wrapper.
     */
    template<typename Type, typename = std::enable_if_t<!std::is_same_v<std::decay_t<Type>, any>>>
    any(Type &&value)
        : any{in_place_type<std::decay_t<Type>>, std::forward<Type>(value)}
    {}

    /**
     * @brief Copy constructor.
     * @param other The instance to copy from.
     */
    any(const any &other)
        : any{}
    {
        if(other.vtable(operation::COPY, other, this)) {
            vtable = other.vtable;
        }
    }

    /**
     * @brief Move constructor.
     * @param other The instance to move from.
     */
    any(any &&other) ENTT_NOEXCEPT
        : any{}
    {
        vtable = std::exchange(other.vtable, &void_vtable);
        vtable(operation::MOVE, other, this);
    }

    /*! @brief Frees the internal storage, whatever it means. */
    ~any() {
        vtable(operation::DTOR, *this, nullptr);
    }

    /**
     * @brief Assignment operator.
     * @param other The instance to assign from.
     * @return This any object.
     */
    any & operator=(any other) {
        swap(*this, other);
        return *this;
    }

    /**
     * @brief Returns the type of the contained object.
     * @return The type of the contained object, if any.
     */
    [[nodiscard]] type_info type() const ENTT_NOEXCEPT {
        type_info info;
        vtable(operation::TYPE, *this, &info);
        return info;
    }

    /**
     * @brief Returns an opaque pointer to the contained instance.
     * @return An opaque pointer the contained instance, if any.
     */
    [[nodiscard]] const void * data() const ENTT_NOEXCEPT {
        return vtable(operation::CADDR, *this, nullptr);
    }

    /*! @copydoc data */
    [[nodiscard]] void * data() ENTT_NOEXCEPT {
        return const_cast<void *>(vtable(operation::ADDR, *this, nullptr));
    }

    /**
     * @brief Replaces the contained object by creating a new instance directly.
     * @tparam Type Type of object to use to initialize the wrapper.
     * @tparam Args Types of arguments to use to construct the new instance.
     * @param args Parameters to use to construct the instance.
     */
    template<typename Type, typename... Args>
    void emplace(Args &&... args) {
        *this = any{in_place_type<Type>, std::forward<Args>(args)...};
    }

    /*! @brief Destroys contained object */
    void reset() {
        *this = any{};
    }

    /**
     * @brief Returns false if a wrapper is empty, true otherwise.
     * @return False if the wrapper is empty, true otherwise.
     */
    [[nodiscard]] explicit operator bool() const ENTT_NOEXCEPT {
        return !(vtable(operation::CADDR, *this, nullptr) == nullptr);
    }

    /**
     * @brief Checks if two wrappers differ in their content.
     * @param other Wrapper with which to compare.
     * @return False if the two objects differ in their content, true otherwise.
     */
    bool operator==(const any &other) const ENTT_NOEXCEPT {
        return type() == other.type() && (vtable(operation::COMP, *this, other.data()) == other.data());
    }

    /**
     * @brief Swaps two any objects.
     * @param lhs A valid any object.
     * @param rhs A valid any object.
     */
    friend void swap(any &lhs, any &rhs) {
        any tmp{};
        lhs.vtable(operation::MOVE, lhs, &tmp);
        rhs.vtable(operation::MOVE, rhs, &lhs);
        lhs.vtable(operation::MOVE, tmp, &rhs);
        std::swap(lhs.vtable, rhs.vtable);
    }

    /**
     * @brief Aliasing constructor.
     * @param other A reference to an object that isn't necessarily initialized.
     * @return An any that shares a reference to an unmanaged object.
     */
    [[nodiscard]] friend any as_ref(any &other) ENTT_NOEXCEPT {
        any ref{};
        other.vtable(operation::REF, other, &ref);
        return ref;
    }

    /*! @copydoc as_ref */
    [[nodiscard]] friend any as_ref(const any &other) ENTT_NOEXCEPT {
        any ref{};
        other.vtable(operation::CREF, other, &ref);
        return ref;
    }

private:
    vtable_type *vtable;
    union { const void *instance; storage_type storage; };
};


/**
 * @brief Checks if two wrappers differ in their content.
 * @param lhs A wrapper, either empty or not.
 * @param rhs A wrapper, either empty or not.
 * @return True if the two wrappers differ in their content, false otherwise.
 */
[[nodiscard]] inline bool operator!=(const any &lhs, const any &rhs) ENTT_NOEXCEPT {
    return !(lhs == rhs);
}


/**
 * @brief Performs type-safe access to the contained object.
 * @tparam Type Type to which conversion is required.
 * @param data Target any object.
 * @return The element converted to the requested type.
 */
template<typename Type>
Type any_cast(const any &data) ENTT_NOEXCEPT {
    const auto * const instance = any_cast<std::remove_reference_t<Type>>(&data);
    ENTT_ASSERT(instance);
    return static_cast<Type>(*instance);
}


/*! @copydoc any_cast */
template<typename Type>
Type any_cast(any &data) ENTT_NOEXCEPT {
    // forces const on non-reference types to make them work also with wrappers for const references
    auto * const instance = any_cast<std::conditional_t<std::is_reference_v<Type>, std::remove_reference_t<Type>, const Type>>(&data);
    ENTT_ASSERT(instance);
    return static_cast<Type>(*instance);
}


/*! @copydoc any_cast */
template<typename Type>
Type any_cast(any &&data) ENTT_NOEXCEPT {
    // forces const on non-reference types to make them work also with wrappers for const references
    auto * const instance = any_cast<std::conditional_t<std::is_reference_v<Type>, std::remove_reference_t<Type>, const Type>>(&data);
    ENTT_ASSERT(instance);
    return static_cast<Type>(std::move(*instance));
}


/*! @copydoc any_cast */
template<typename Type>
const Type * any_cast(const any *data) ENTT_NOEXCEPT {
    return (data->type() == type_id<Type>() ? static_cast<const Type *>(data->data()) : nullptr);
}


/*! @copydoc any_cast */
template<typename Type>
Type * any_cast(any *data) ENTT_NOEXCEPT {
    // last attempt to make wrappers for const references return their values
    return (data->type() == type_id<Type>() ? static_cast<Type *>(static_cast<constness_as_t<any, Type> *>(data)->data()) : nullptr);
}


}


#endif
